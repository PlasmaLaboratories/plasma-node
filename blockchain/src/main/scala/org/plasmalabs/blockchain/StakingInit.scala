package org.plasmalabs.blockchain

import cats.*
import cats.data.OptionT
import cats.effect.*
import cats.effect.implicits.*
import cats.implicits.*
import com.google.protobuf.ByteString
import fs2.Chunk
import fs2.io.file.{Files, Path}
import org.plasmalabs.algebras.ClockAlgebra.implicits.*
import org.plasmalabs.algebras.{ClockAlgebra, Stats}
import org.plasmalabs.blockchain.algebras.NodeMetadataAlgebra
import org.plasmalabs.codecs.bytes.tetra.instances.*
import org.plasmalabs.config.ApplicationConfig
import org.plasmalabs.consensus.algebras.*
import org.plasmalabs.consensus.models.{VrfConfig, *}
import org.plasmalabs.interpreters.CatsSecureStore
import org.plasmalabs.minting.algebras.StakingAlgebra
import org.plasmalabs.minting.interpreters.*
import org.plasmalabs.models.protocol.BigBangConstants.*
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.models.{LockAddress, TransactionId}
import org.plasmalabs.sdk.syntax.*
import org.plasmalabs.typeclasses.implicits.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object StakingInit {

  final val KesDirectoryName = "kes"
  final val OperatorKeyName = "operator-key.ed25519.sk"
  final val VrfKeyName = "vrf-key.ed25519vrf.sk"
  final val RegistrationTxName = "registration.transaction.pbuf"

  implicit private def logger[F[_]: Sync]: Logger[F] =
    Slf4jLogger.getLoggerFromName[F]("Node.StakingInit")

  /**
   * Inspects the given stakingDir for the expected keys/files.  If the expected files exist, `true` is returned.
   */
  def stakingIsInitialized[F[_]: Async](stakingDir: Path): F[Boolean] =
    Files
      .forAsync[F]
      .exists(stakingDir)
      .ifM(
        Files
          .forAsync[F]
          .list(stakingDir)
          .compile
          .toList
          .map(files =>
            files.exists(_.endsWith(KesDirectoryName)) &&
            files.exists(_.endsWith(VrfKeyName))
          ),
        false.pure[F]
      )

  /**
   * Initializes a Staking object from existing files on disk.  The files are expected to be in the format created
   * by the "Registration" CLI process.
   */
  def makeStakingFromDisk[F[_]: Async: Stats](
    stakingDir:               Path,
    rewardAddress:            LockAddress,
    configuredStakingAddress: Option[StakingAddress],
    clock:                    ClockAlgebra[F],
    etaCalculation:           EtaCalculationAlgebra[F],
    consensusValidationState: ConsensusValidationStateAlgebra[F],
    leaderElectionThreshold:  LeaderElectionValidationAlgebra[F],
    cryptoResources:          CryptoResources[F],
    protocol:                 ApplicationConfig.Node.Protocol,
    vrfConfig:                VrfConfig,
    metadata:                 NodeMetadataAlgebra[F],
    localChain:               LocalChainAlgebra[F],
    fetchHeader:              BlockId => F[BlockHeader],
    fetchBody:                BlockId => F[BlockBody],
    fetchTransaction:         TransactionId => F[IoTransaction]
  ): Resource[F, StakingAlgebra[F]] =
    for {
      _         <- Logger[F].info(show"Loading registered staker from disk at path=$stakingDir").toResource
      genesisId <- localChain.genesis.map(_.slotId.blockId).toResource
      genesisNetworkId <- OptionT(
        fetchBody(genesisId)
          .flatMap(
            _.transactionIds.collectFirstSomeM(transactionId =>
              fetchTransaction(transactionId).map(_.outputs.headOption.map(_.address.network))
            )
          )
      ).getOrRaise(new IllegalStateException("Could not determine networkId from genesis")).toResource
      _ <- Async[F]
        .raiseWhen(genesisNetworkId != rewardAddress.network)(
          new IllegalArgumentException("Configured staking rewardAddress belongs to a different network")
        )
        .toResource
      kesPath <- Sync[F].delay(stakingDir / KesDirectoryName).toResource
      _ <- Files
        .forAsync[F]
        .list(kesPath)
        .compile
        .count
        .flatMap(kesKeyCount =>
          MonadThrow[F]
            .raiseWhen(kesKeyCount != 1)(
              new IllegalArgumentException(s"Expected exactly one KES key in secure store but found $kesKeyCount.")
            )
        )
        .toResource
      readFile = (p: Path) => Files.forAsync[F].readAll(p).compile.to(Chunk)
      vrfSK <- readFile(stakingDir / VrfKeyName).map(_.toArray).toResource
      vrfVK <- cryptoResources.ed25519VRF.use(e => Sync[F].delay(e.getVerificationKey(vrfSK))).toResource
      stakingAddress <- OptionT
        .fromOption[Resource[F, *]](configuredStakingAddress)
        .orElse(
          // In older implementations, the IoTransaction containing the registration must exist on disk.
          // To support this behavior, fallback to reading it if the user doesn't configure the staking address
          OptionT
            .liftF(readFile(stakingDir / RegistrationTxName).map(_.toArray).map(IoTransaction.parseFrom).toResource)
            .semiflatMap(transaction =>
              OptionT
                .fromOption[Resource[F, *]](
                  transaction.outputs.headOption.flatMap(_.value.value.topl.flatMap(_.registration.map(_.address)))
                )
                .getOrRaise(new IllegalArgumentException("Registration Transaction is invalid"))
            )
            .semiflatTap(_ =>
              Logger[F]
                .warn(
                  "Registration loaded from local transaction which is deprecated.  Please configure the node's staking address."
                )
                .toResource
            )
        )
        .getOrRaise(new IllegalArgumentException("Staking address not configured"))
      staking <- makeStaking(
        stakingDir,
        ByteString.copyFrom(vrfSK),
        ByteString.copyFrom(vrfVK),
        stakingAddress,
        rewardAddress,
        clock,
        etaCalculation,
        consensusValidationState,
        leaderElectionThreshold,
        cryptoResources,
        protocol,
        vrfConfig,
        metadata,
        localChain,
        fetchHeader,
        fetchBody,
        fetchTransaction
      )
    } yield staking

  /**
   * Initializes a Staking object from the given raw VRF and staking address information
   */
  private def makeStaking[F[_]: Async: Stats](
    stakingDir:               Path,
    vrfSK:                    ByteString,
    vrfVK:                    ByteString,
    stakingAddress:           StakingAddress,
    rewardAddress:            LockAddress,
    clock:                    ClockAlgebra[F],
    etaCalculation:           EtaCalculationAlgebra[F],
    consensusValidationState: ConsensusValidationStateAlgebra[F],
    leaderElectionThreshold:  LeaderElectionValidationAlgebra[F],
    cryptoResources:          CryptoResources[F],
    protocol:                 ApplicationConfig.Node.Protocol,
    vrfConfig:                VrfConfig,
    metadata:                 NodeMetadataAlgebra[F],
    localChain:               LocalChainAlgebra[F],
    fetchHeader:              BlockId => F[BlockHeader],
    fetchBody:                BlockId => F[BlockBody],
    fetchTransaction:         TransactionId => F[IoTransaction]
  ): Resource[F, StakingAlgebra[F]] =
    for {
      (registrationHeader, registrationTransaction) <-
        // Check the metadata store to see if there already exists a cached entry.  First, double-check that the cached
        // entry is associated with the expected transaction ID
        OptionT(metadata.readStakingRegistrationTransactionId)
          .semiflatMap(fetchTransaction)
          .filter(containsActivationRegistrationFor(stakingAddress))
          .flatMap(registrationTransaction =>
            // If the transaction ID matches, also fetch the corresponding Block ID
            OptionT(metadata.readStakingRegistrationBlockId)
              .filterF(
                fetchBody(_).map(_.transactionIds.contains(registrationTransaction.id))
              )
              .semiflatMap(fetchHeader)
              .tupleRight(registrationTransaction)
          )
          // If the metadata store did not contain an entry, go and find the header on the chain
          .getOrElseF(
            findStakerRegistration(localChain, fetchHeader, fetchBody, fetchTransaction)(stakingAddress)
              // Once the header has been found, cache its ID in the metadata store
              .flatTap { case (header, transaction) =>
                metadata.setStakingRegistrationTransactionId(transaction.id) *>
                metadata.setStakingRegistrationBlockId(header.id)
              }
          )
          .toResource
      _ <- Logger[F]
        .info(
          show"Found registration" +
          show" transactionId=${registrationTransaction.id}" +
          show" in blockId=${registrationHeader.id}" +
          show" slot=${registrationHeader.slot}" +
          show" height=${registrationHeader.height}"
        )
        .toResource
      registrationEpoch <- clock.epochOf(registrationHeader.slot).toResource
      // Stakers who are registered in the genesis block have an activation epoch of 0.  Everyone else has an
      // activation epoch = registration epoch + 2
      activationEpoch = if (registrationHeader.height == BigBangHeight) 0L else registrationEpoch + 2
      beginSlot <- clock
        .epochRange(activationEpoch)
        .map(_.start)
        .toResource
      activationPeriod <- clock.operationalPeriodOf(beginSlot).toResource
      globalSlot       <- clock.globalSlot.toResource
      _ <- Async[F]
        .whenA(beginSlot > globalSlot)(
          Logger[F].info(s"Delaying staking procedures until slot=$beginSlot") >>
          clock.delayedUntilSlot(beginSlot)
        )
        .toResource
      _ <- Logger[F]
        .info(
          show"Constructing staker with" +
          show" activationSlot=$beginSlot" +
          show" activationPeriod=$activationPeriod"
        )
        .toResource
      kesPath     <- Sync[F].delay(stakingDir / KesDirectoryName).toResource
      secureStore <- CatsSecureStore.make[F](kesPath.toNioPath)
      vrfCalculator <- VrfCalculator.make[F](
        vrfSK,
        cryptoResources.ed25519VRF,
        protocol.vrfCacheSize
      )

      operationalKeys <- OperationalKeyMaker
        .make[F](
          activationOperationalPeriod = activationPeriod,
          stakingAddress,
          vrfConfig,
          secureStore = secureStore,
          clock = clock,
          vrfCalculator = vrfCalculator,
          leaderElectionThreshold,
          consensusValidationState,
          cryptoResources.kesProduct,
          cryptoResources.ed25519
        )

      staking <- Staking.make(
        stakingAddress,
        rewardAddress,
        vrfVK,
        operationalKeys,
        consensusValidationState,
        etaCalculation,
        cryptoResources.ed25519,
        cryptoResources.blake2b256,
        vrfCalculator,
        leaderElectionThreshold
      )
    } yield staking

  /**
   * Scans the canonical chain for the staker's activation registration transaction.
   * @return a tuple (The containg header, the registration transaction)
   */
  def findStakerRegistration[F[_]: Async](
    localChain:       LocalChainAlgebra[F],
    fetchHeader:      BlockId => F[BlockHeader],
    fetchBody:        BlockId => F[BlockBody],
    fetchTransaction: TransactionId => F[IoTransaction]
  )(stakingAddress: StakingAddress): F[(BlockHeader, IoTransaction)] =
    Logger[F].info(
      show"Searching local chain for registration transaction containing stakingAddress=$stakingAddress"
    ) >>
    OptionT(
      fs2.Stream
        .eval(localChain.head)
        .map(_.slotId.blockId)
        .flatMap(head =>
          fs2.Stream.unfoldLoopEval(head)(id =>
            // TODO optimization: Stop scanning if the headers become older than the maximum KES evolutions would permit
            fetchHeader(id).map(h => (h, Option.when(h.height > BigBangHeight)(h.parentHeaderId)))
          )
        )
        // In addition to scanning the chain's history, also search new block adoptions
        .merge(fs2.Stream.force(localChain.adoptions).evalMap(fetchHeader))
        .flatMap(header =>
          fs2.Stream
            .evalSeq(fetchBody(header.id).map(_.transactionIds))
            .evalMap(fetchTransaction)
            // Find the "activation transaction", meaning the first transaction that registered the address
            // Stake-shifting transactions (i.e. existing registration, but new stake quantity) are skipped
            .find(containsActivationRegistrationFor(stakingAddress))
            .tupleLeft(header)
        )
        .head
        .compile
        .last
    ).getOrRaise(
      new IllegalStateException(
        "StakingAddress not found in canonical chain.  Make sure your registration transaction is broadcasted" +
        " and confirmed before launching the node with staking enabled."
      )
    )

  private def containsActivationRegistrationFor(stakingAddress: StakingAddress)(transaction: IoTransaction) =
    transaction.outputs.exists(_.value.value.topl.exists(_.registration.exists(_.address == stakingAddress))) &&
    !transaction.inputs.exists(_.value.value.topl.exists(_.registration.exists(_.address == stakingAddress)))
}
