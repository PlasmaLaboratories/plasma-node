package org.plasmalabs.node

import cats.data.OptionT
import cats.effect.implicits.*
import cats.effect.std.{Random, SecureRandom}
import cats.effect.{Async, IO, Resource, Sync}
import cats.implicits.*
import com.google.protobuf.ByteString
import com.typesafe.config.Config
import fs2.io.file.Path
import kamon.Kamon
import org.plasmalabs.algebras.Stats
import org.plasmalabs.blockchain.*
import org.plasmalabs.blockchain.interpreters.{EpochDataEventSourcedState, EpochDataInterpreter, NodeMetadata}
import org.plasmalabs.buildinfo.node.BuildInfo
import org.plasmalabs.catsutils.*
import org.plasmalabs.codecs.bytes.tetra.instances.*
import org.plasmalabs.common.application.IOBaseApp
import org.plasmalabs.config.ApplicationConfig
import org.plasmalabs.config.ApplicationConfig.Node.KnownPeer
import org.plasmalabs.consensus.*
import org.plasmalabs.consensus.interpreters.*
import org.plasmalabs.consensus.interpreters.CrossEpochEventSourceState.VotingData
import org.plasmalabs.consensus.models.{BlockId, VrfConfig}
import org.plasmalabs.crypto.hash.Blake2b512
import org.plasmalabs.crypto.signing.Ed25519
import org.plasmalabs.eventtree.ParentChildTree
import org.plasmalabs.grpc.{HealthCheckGrpc, *}
import org.plasmalabs.healthcheck.HealthCheck
import org.plasmalabs.indexer.*
import org.plasmalabs.interpreters.*
import org.plasmalabs.ledger.LedgerImpl
import org.plasmalabs.ledger.interpreters.*
import org.plasmalabs.ledger.interpreters.ProposalEventSourceState.ProposalData
import org.plasmalabs.models.ProposalConfig
import org.plasmalabs.models.p2p.*
import org.plasmalabs.models.protocol.BigBangConstants.*
import org.plasmalabs.models.utility.*
import org.plasmalabs.models.utility.HasLength.instances.byteStringLength
import org.plasmalabs.networking.p2p.LocalPeer
import org.plasmalabs.node.cli.ConfiguredCliApp
import org.plasmalabs.numerics.interpreters.{ExpInterpreter, Log1pInterpreter}
import org.plasmalabs.sdk.validation.{TransactionCostCalculatorInterpreter, TransactionCostConfig}
import org.plasmalabs.typeclasses.implicits.*
import org.plasmalabs.version.VersionReplicator
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Instant
import scala.concurrent.duration.*

import ApplicationConfigOps.*

object NodeApp extends AbstractNodeApp

abstract class AbstractNodeApp
    extends IOBaseApp[Args, ApplicationConfig](
      createArgs = a => IO.delay(Args.parse(a)),
      createConfig = IOBaseApp.createTypesafeConfig(_, AbstractNodeApp.ConfigFileEnvironmentVariable.some),
      parseConfig = (args, conf) => IO.delay(ApplicationConfigOps.unsafe(args, conf)),
      preInitFunction = config => IO.delay(if (config.kamon.enable) Kamon.init())
    ) {

  def run(cmdArgs: Args, config: Config, appConfig: ApplicationConfig): IO[Unit] =
    if (cmdArgs.startup.cli) new ConfiguredCliApp(appConfig).run
    else if (cmdArgs.startup.idle) new IdleApp(appConfig).run
    else if (cmdArgs.startup.pruneDir.isDefined) new PrunedDataStoresApp(appConfig, cmdArgs.startup.pruneDir.get).run
    else new ConfiguredNodeApp(cmdArgs, appConfig).run
}

object AbstractNodeApp {
  final val ConfigFileEnvironmentVariable = "NODE_CONFIG_FILE"
}

class ConfiguredNodeApp(args: Args, appConfig: ApplicationConfig) {

  type F[A] = IO[A]

  def run: IO[Unit] = applicationResource.use_

  // scalastyle:off method.length
  private def applicationResource: Resource[F, Unit] =
    for {
      given Async[F]  <- Resource.pure(implicitly[Async[F]])
      given Logger[F] <- Resource.pure(Slf4jLogger.getLoggerFromName[F]("Node.Node"))

      _ <- Sync[F].delay(LoggingUtils.initialize(args)).toResource
      _ <- Logger[F].info(show"Launching node with args=$args").toResource
      _ <- Logger[F].info(show"Node configuration=$appConfig").toResource

      given Stats[F] <- KamonStatsRef.make[F]

      cryptoResources            <- CryptoResources.make[F].toResource
      (bigBangBlock, dataStores) <- DataStoresInit.initializeData(appConfig)

      metadata <- NodeMetadata.make(dataStores.metadata)
      _        <- metadata.setAppVersion(BuildInfo.version).toResource
      _ <- OptionT(metadata.readInitTime)
        .flatTapNone(IO.realTime.map(_.toMillis).flatMap(metadata.setInitTime))
        .value
        .toResource

      given Random[F] <- SecureRandom.javaSecuritySecureRandom[F].toResource

      proposalConfig <- ProposalConfig().pure[F].toResource
      p2pSK <- OptionT(metadata.readP2PSK)
        .getOrElseF(
          Random[F]
            .nextBytes(32)
            .flatMap(seed =>
              cryptoResources.ed25519
                .use(e => Sync[F].delay(e.deriveSecretKeyFromSeed(seed)))
                .map(_.bytes)
                .map(ByteString.copyFrom)
            )
            .flatTap(metadata.setP2PSK)
        )
        .toResource
      p2pVK <- cryptoResources.ed25519
        .use(e => Sync[F].delay(e.getVerificationKey(Ed25519.SecretKey(p2pSK.toByteArray))))
        .map(_.bytes)
        .map(ByteString.copyFrom)
        .toResource

      localPeer = LocalPeer(
        RemoteAddress(appConfig.node.p2p.bindHost, appConfig.node.p2p.bindPort),
        p2pVK,
        p2pSK
      )

      bigBangBlockId = bigBangBlock.header.id
      bigBangSlotData <- dataStores.slotData.getOrRaise(bigBangBlockId).toResource
      _ <- Logger[F].info(show"Big Bang Block id=$bigBangBlockId timestamp=${bigBangBlock.header.timestamp}").toResource

      stakingDir = Path(interpolateBlockId(bigBangBlockId)(appConfig.node.staking.directory))
      _ <- Logger[F].info(show"Using stakingDir=$stakingDir").toResource

      currentEventIdGetterSetters = new CurrentEventIdGetterSetters[F](dataStores.currentEventIds)

      canonicalHeadId       <- currentEventIdGetterSetters.canonicalHead.get().toResource
      canonicalHeadSlotData <- dataStores.slotData.getOrRaise(canonicalHeadId).toResource
      canonicalHead         <- dataStores.headers.getOrRaise(canonicalHeadId).toResource
      _ <- Logger[F]
        .info(
          show"Canonical head" +
          show" id=$canonicalHeadId" +
          show" height=${canonicalHeadSlotData.height}" +
          show" slot=${canonicalHeadSlotData.slotId.slot}"
        )
        .toResource

      blockIdTree <- ParentChildTree.FromReadWrite
        .make[F, BlockId](
          dataStores.parentChildTree.get,
          dataStores.parentChildTree.put,
          bigBangBlock.header.parentHeaderId
        )
        .toResource

      // Start the supporting interpreters
      txIdToBlockIdTree <- TxIdToBlockIdTree
        .make(
          canonicalHeadId.pure[F],
          dataStores.bodies.get,
          dataStores.txIdToBlockId,
          blockIdTree
        )
        .toResource
      blockHeightTreeLocal <- BlockHeightTree
        .make[F](
          dataStores.blockHeightTreeLocal,
          currentEventIdGetterSetters.blockHeightTreeLocal.get(),
          dataStores.slotData,
          blockIdTree,
          currentEventIdGetterSetters.blockHeightTreeLocal.set
        )
        .toResource
      blockHeightTreeP2P <- BlockHeightTree
        .make[F](
          dataStores.blockHeightTreeP2P,
          currentEventIdGetterSetters.blockHeightTreeP2P.get(),
          dataStores.slotData,
          blockIdTree,
          currentEventIdGetterSetters.blockHeightTreeP2P.set
        )
        .toResource
      _ <- OptionT(blockHeightTreeLocal.useStateAt(canonicalHeadId)(_.apply(BigBangHeight)))
        .ensure(new IllegalStateException("The configured genesis block does not match the stored genesis block."))(
          _ === bigBangBlockId
        )
        .value
        .toResource
      bigBangProtocol <-
        BigBang
          .extractProtocol(bigBangBlock)
          .leftMap(error =>
            new IllegalArgumentException(s"Genesis block contained invalid protocol settings. reason=$error")
          )
          .pure[F]
          .rethrow
          .flatMap(protocol =>
            appConfig.node
              .protocols(0)
              .epochLengthOverride
              .foldLeftM(
                protocol
              )((protocol, lengthOverride) =>
                Logger[F].warn(s"Overriding epoch length to $lengthOverride slots") >>
                protocol
                  .copy(epochLengthOverride = appConfig.node.protocols(0).epochLengthOverride)
                  .pure[F]
              )
          )
          .flatTap(p => p.validation.leftMap(new IllegalArgumentException(_)).pure[F].rethrow)
          .toResource
      _ <- Logger[F]
        .info(
          "Protocol" +
          s" epochLength=${bigBangProtocol.epochLength}" +
          s" operationalPeriodLength=${bigBangProtocol.operationalPeriodLength}" +
          s" chainSelectionSWindow=${bigBangProtocol.chainSelectionSWindow}" +
          s" settings=$bigBangProtocol"
        )
        .toResource
      vrfConfig = VrfConfig(
        bigBangProtocol.vrfLddCutoff,
        bigBangProtocol.vrfPrecision,
        bigBangProtocol.vrfBaselineDifficulty,
        bigBangProtocol.vrfAmplitude
      )
      ntpClockSkewer <- NtpClockSkewer
        .make[F](appConfig.node.ntp.server, appConfig.node.ntp.refreshInterval, appConfig.node.ntp.timeout)
      clock <- SchedulerClock.make[F](
        bigBangProtocol.slotDuration,
        bigBangProtocol.epochLength,
        bigBangProtocol.operationalPeriodLength,
        Instant.ofEpochMilli(bigBangBlock.header.timestamp),
        bigBangProtocol.forwardBiasedSlotWindow,
        ntpClockSkewer
      )
      globalSlot <- clock.globalSlot.toResource
      _          <- Logger[F].info(show"globalSlot=$globalSlot").toResource
      etaCalculation <-
        EtaCalculation
          .make(
            dataStores.slotData.getOrRaise,
            clock,
            Sized.strictUnsafe(bigBangBlock.header.eligibilityCertificate.eta),
            cryptoResources.blake2b256,
            cryptoResources.blake2b512
          )
          .toResource
      leaderElection <- makeLeaderElectionThreshold(
        cryptoResources.blake2b512,
        vrfConfig,
        bigBangProtocol.slotGapLeaderElection
      ).toResource

      epochBoundariesStateLocal <- EpochBoundariesEventSourcedState
        .make[F](
          clock,
          currentEventIdGetterSetters.epochBoundariesLocal.get(),
          blockIdTree,
          currentEventIdGetterSetters.epochBoundariesLocal.set,
          dataStores.epochBoundariesLocal.pure[F],
          dataStores.slotData.getOrRaise
        )
        .toResource
      consensusDataStateLocal <-
        ConsensusDataEventSourcedState
          .make[F](
            currentEventIdGetterSetters.consensusDataLocal.get(),
            blockIdTree,
            currentEventIdGetterSetters.consensusDataLocal.set,
            ConsensusDataEventSourcedState
              .ConsensusData(dataStores.activeStakeLocal, dataStores.inactiveStakeLocal, dataStores.registrationsLocal)
              .pure[F],
            dataStores.bodies.getOrRaise,
            dataStores.transactions.getOrRaise
          )
          .toResource
      consensusValidationStateLocal <- ConsensusValidationState
        .make[F](bigBangBlockId, epochBoundariesStateLocal, consensusDataStateLocal, clock)
        .toResource

      epochBoundariesStateP2P <- EpochBoundariesEventSourcedState
        .make[F](
          clock,
          currentEventIdGetterSetters.epochBoundariesP2P.get(),
          blockIdTree,
          currentEventIdGetterSetters.epochBoundariesP2P.set,
          dataStores.epochBoundariesP2P.pure[F],
          dataStores.slotData.getOrRaise
        )
        .toResource
      consensusDataStateP2P <-
        ConsensusDataEventSourcedState
          .make[F](
            currentEventIdGetterSetters.consensusDataP2P.get(),
            blockIdTree,
            currentEventIdGetterSetters.consensusDataP2P.set,
            ConsensusDataEventSourcedState
              .ConsensusData(dataStores.activeStakeP2P, dataStores.inactiveStakeP2P, dataStores.registrationsP2P)
              .pure[F],
            dataStores.bodies.getOrRaise,
            dataStores.transactions.getOrRaise
          )
          .toResource
      consensusValidationStateP2P <- ConsensusValidationState
        .make[F](bigBangBlockId, epochBoundariesStateP2P, consensusDataStateP2P, clock)
        .toResource

      chainSelection = ChainSelection
        .make[F](
          cryptoResources.blake2b512,
          bigBangProtocol.chainSelectionKLookback,
          bigBangProtocol.chainSelectionSWindow
        )
      localChain <-
        LocalChain.make(
          bigBangSlotData,
          canonicalHeadSlotData,
          chainSelection,
          currentEventIdGetterSetters.canonicalHead.set,
          blockHeightTreeLocal,
          dataStores.slotData.getOrRaise
        )
      staking =
        OptionT
          .liftF(StakingInit.stakingIsInitialized[F](stakingDir).toResource)
          .filter(identity)
          .flatTapNone(Logger[F].warn("Staking directory is empty.  Continuing in relay-only mode.").toResource)
          .semiflatMap { _ =>
            // Construct a separate threshold calculator instance with a separate cache to avoid
            // polluting the staker's cache with remote block eligibilities
            makeLeaderElectionThreshold(
              cryptoResources.blake2b512,
              vrfConfig,
              bigBangProtocol.slotGapLeaderElection
            ).toResource
              .flatMap(leaderElectionThreshold =>
                StakingInit
                  .makeStakingFromDisk(
                    stakingDir,
                    appConfig.node.staking.rewardAddress,
                    appConfig.node.staking.stakingAddress,
                    clock,
                    etaCalculation,
                    consensusValidationStateLocal,
                    leaderElectionThreshold,
                    cryptoResources,
                    bigBangProtocol,
                    vrfConfig,
                    metadata,
                    localChain,
                    dataStores.headers.getOrRaise,
                    dataStores.bodies.getOrRaise,
                    dataStores.transactions.getOrRaise
                  )
              )
          }
          .value

      eligibilityCache <-
        EligibilityCache
          .make[F](appConfig.node.cache.eligibilities.maximumEntries.toInt)
          .evalTap(
            EligibilityCache.repopulate(
              _,
              appConfig.node.cache.eligibilities.maximumEntries.toInt,
              canonicalHead,
              dataStores.headers.getOrRaise
            )
          )
      (boxStateLocal, boxStateStateLocal) <- BoxState
        .make(
          currentEventIdGetterSetters.boxStateLocal.get(),
          dataStores.bodies.getOrRaise,
          dataStores.transactions.getOrRaise,
          blockIdTree,
          currentEventIdGetterSetters.boxStateLocal.set,
          dataStores.spendableBoxIdsLocal.pure[F]
        )
        .toResource
      (boxStateP2P, boxStateStateP2P) <- BoxState
        .make(
          currentEventIdGetterSetters.boxStateP2P.get(),
          dataStores.bodies.getOrRaise,
          dataStores.transactions.getOrRaise,
          blockIdTree,
          currentEventIdGetterSetters.boxStateP2P.set,
          dataStores.spendableBoxIdsP2P.pure[F]
        )
        .toResource
      (registrationAccumulatorLocal, registrationAccumulatorStateLocal) <- RegistrationAccumulator
        .make[F](
          currentEventIdGetterSetters.registrationAccumulatorLocal.get(),
          dataStores.bodies.getOrRaise,
          dataStores.transactions.getOrRaise,
          blockIdTree,
          currentEventIdGetterSetters.registrationAccumulatorLocal.set,
          dataStores.registrationAccumulatorLocal.pure[F]
        )
      (registrationAccumulatorP2P, registrationAccumulatorStateP2P) <- RegistrationAccumulator
        .make[F](
          currentEventIdGetterSetters.registrationAccumulatorP2P.get(),
          dataStores.bodies.getOrRaise,
          dataStores.transactions.getOrRaise,
          blockIdTree,
          currentEventIdGetterSetters.registrationAccumulatorP2P.set,
          dataStores.registrationAccumulatorP2P.pure[F]
        )

      indexerOpt <- OptionT
        .whenF(appConfig.indexer.enable)(
          Indexer
            .make[F](
              appConfig.node.rpc.bindHost,
              appConfig.node.rpc.bindPort,
              nodeRpcTls = false,
              Some(appConfig.indexer.orientDbDirectory)
                .filterNot(_.isEmpty)
                .getOrElse(dataStores.baseDirectory./("orient-db").toString),
              appConfig.indexer.orientDbPassword,
              ttlCacheCheck = appConfig.indexer.ttlCacheCheck
            )
        )
        .value
      indexerServices <- indexerOpt.toList.flatTraverse(indexer =>
        IndexerGrpc.Server.services(
          indexer.blockFetcher,
          indexer.transactionFetcher,
          indexer.vertexFetcher,
          indexer.valueFetcher,
          indexer.replicatorStatus
        )
      )
      healthCheck    <- HealthCheck.make[F]()
      healthServices <- HealthCheckGrpc.Server.services(healthCheck.healthChecker)

      protocolConfig <- ProtocolConfiguration.make[F](List(bigBangProtocol.nodeConfig(0)))

      transactionRewardCalculator <- TransactionRewardCalculator.make[F]

      epochDataEventSourcedStateLocal <- EpochDataEventSourcedState.make[F](
        currentEventIdGetterSetters.epochDataLocal.get(),
        bigBangBlockId,
        blockIdTree,
        currentEventIdGetterSetters.epochDataLocal.set,
        dataStores.epochData.pure[F],
        clock,
        dataStores.headers.getOrRaise,
        dataStores.bodies.getOrRaise,
        dataStores.transactions.getOrRaise,
        transactionRewardCalculator,
        epochBoundariesStateLocal,
        consensusDataStateLocal
      )

      epochDataEventSourcedStateP2P <- EpochDataEventSourcedState.make[F](
        currentEventIdGetterSetters.epochDataP2P.get(),
        bigBangBlockId,
        blockIdTree,
        currentEventIdGetterSetters.epochDataP2P.set,
        dataStores.epochData.pure[F],
        clock,
        dataStores.headers.getOrRaise,
        dataStores.bodies.getOrRaise,
        dataStores.transactions.getOrRaise,
        transactionRewardCalculator,
        epochBoundariesStateP2P,
        consensusDataStateP2P
      )

      epochData <- EpochDataInterpreter
        .make[F](Sync[F].defer(localChain.head).map(_.slotId.blockId), epochDataEventSourcedStateLocal)

      softwareVersion <- OptionT
        .whenF(appConfig.node.versionInfo.enable)(
          VersionReplicator.make[F](metadata, appConfig.node.versionInfo.uri)
        )
        .value

      costCalculator = TransactionCostCalculatorInterpreter.make[F](TransactionCostConfig())
      (mempool, mempoolState) <- Mempool.make[F](
        currentEventIdGetterSetters.mempool.get(),
        dataStores.bodies.getOrRaise,
        dataStores.transactions.getOrRaise,
        blockIdTree,
        currentEventIdGetterSetters.mempool.set,
        clock,
        id => Logger[F].info(show"Expiring transaction id=$id"),
        appConfig.node.mempool.defaultExpirationSlots,
        transactionRewardCalculator,
        costCalculator
      )

      versionInfoLocal <- VersionInfo.make(dataStores.versioningDataStoresLocal.epochToActiveVersionStorage).toResource

      proposalLocalInitialState = new ProposalData[F](
        dataStores.versioningDataStoresLocal.idToProposal,
        dataStores.versioningDataStoresLocal.epochToCreatedProposalIds
      )
      proposalEventLocal <- ProposalEventSourceState
        .make[F](
          currentEventIdGetterSetters.proposalLocal.get(),
          blockIdTree,
          currentEventIdGetterSetters.proposalLocal.set,
          proposalLocalInitialState.pure[F],
          clock,
          dataStores.headers.getOrRaise,
          dataStores.bodies.getOrRaise,
          dataStores.transactions.getOrRaise,
          proposalConfig
        )
        .toResource

      votingLocalInitialState = new VotingData[F](
        dataStores.versioningDataStoresLocal.epochToProposalIds,
        dataStores.versioningDataStoresLocal.proposalVoting,
        dataStores.versioningDataStoresLocal.epochToCreatedVersionIds,
        dataStores.versioningDataStoresLocal.epochToVersionIds,
        dataStores.versioningDataStoresLocal.versionIdToProposal,
        dataStores.versioningDataStoresLocal.versionCounter,
        dataStores.versioningDataStoresLocal.versionVoting,
        versionInfoLocal
      )

      crossEpochForkLocal <- CrossEpochEventSourceState
        .make[F](
          currentEventIdGetterSetters.crossEpochForkLocal.get(),
          blockIdTree,
          currentEventIdGetterSetters.crossEpochForkLocal.set,
          votingLocalInitialState.pure[F],
          clock,
          dataStores.headers.getOrRaise,
          epochBoundariesStateLocal,
          epochDataEventSourcedStateLocal,
          proposalEventLocal,
          bigBangBlock.header.id,
          proposalConfig
        )
        .toResource

      votingForkLocal <- VotingEventSourceState
        .make[F](
          currentEventIdGetterSetters.votingForkLocal.get(),
          blockIdTree,
          currentEventIdGetterSetters.votingForkLocal.set,
          VotingEventSourceState.State[F]().pure[F],
          clock,
          dataStores.headers.getOrRaise,
          crossEpochForkLocal
        )
        .toResource

      versionInfoP2P <- VersionInfo.make(dataStores.versioningDataStoresP2P.epochToActiveVersionStorage).toResource

      proposalP2PInitialState = new ProposalData[F](
        dataStores.versioningDataStoresP2P.idToProposal,
        dataStores.versioningDataStoresP2P.epochToCreatedProposalIds
      )
      proposalEventP2P <- ProposalEventSourceState
        .make[F](
          currentEventIdGetterSetters.proposalP2P.get(),
          blockIdTree,
          currentEventIdGetterSetters.proposalP2P.set,
          proposalP2PInitialState.pure[F],
          clock,
          dataStores.headers.getOrRaise,
          dataStores.bodies.getOrRaise,
          dataStores.transactions.getOrRaise,
          proposalConfig
        )
        .toResource

      votingP2PInitialState = new VotingData[F](
        dataStores.versioningDataStoresP2P.epochToProposalIds,
        dataStores.versioningDataStoresP2P.proposalVoting,
        dataStores.versioningDataStoresP2P.epochToCreatedVersionIds,
        dataStores.versioningDataStoresP2P.epochToVersionIds,
        dataStores.versioningDataStoresP2P.versionIdToProposal,
        dataStores.versioningDataStoresP2P.versionCounter,
        dataStores.versioningDataStoresP2P.versionVoting,
        versionInfoP2P
      )

      crossEpochForkP2P <- CrossEpochEventSourceState
        .make[F](
          currentEventIdGetterSetters.crossEpochForkP2P.get(),
          blockIdTree,
          currentEventIdGetterSetters.crossEpochForkP2P.set,
          votingP2PInitialState.pure[F],
          clock,
          dataStores.headers.getOrRaise,
          epochBoundariesStateP2P,
          epochDataEventSourcedStateP2P,
          proposalEventP2P,
          bigBangBlock.header.id,
          proposalConfig
        )
        .toResource

      votingForkP2P <- VotingEventSourceState
        .make[F](
          currentEventIdGetterSetters.votingForkP2P.get(),
          blockIdTree,
          currentEventIdGetterSetters.votingForkP2P.set,
          VotingEventSourceState.State[F]().pure[F],
          clock,
          dataStores.headers.getOrRaise,
          crossEpochForkLocal
        )
        .toResource

      eventSourcedStates = EventSourcedStates[F](
        epochDataEventSourcedStateLocal,
        epochDataEventSourcedStateP2P,
        blockHeightTreeLocal,
        blockHeightTreeP2P,
        consensusDataStateLocal,
        consensusDataStateP2P,
        epochBoundariesStateLocal,
        epochBoundariesStateP2P,
        boxStateStateLocal,
        boxStateStateP2P,
        mempoolState,
        registrationAccumulatorStateLocal,
        registrationAccumulatorStateP2P,
        txIdToBlockIdTree,
        crossEpochForkLocal,
        crossEpochForkP2P,
        votingForkLocal,
        votingForkP2P,
        proposalEventLocal,
        proposalEventP2P
      )

      _ <- Logger[F].info(show"Updating EventSourcedStates to id=$canonicalHeadId").toResource
      _ <- eventSourcedStates
        .updateAllStatesTo(canonicalHeadId)
        .logDuration("EventSourcedStates Update")
        .warnIfSlow("EventSourcedStates Update", 5.seconds)
        .toResource

      p2pConfig = appConfig.node.p2p

      validatorsLocal <- Validators.make[F](
        cryptoResources,
        dataStores,
        bigBangBlockId,
        eligibilityCache,
        etaCalculation,
        consensusValidationStateLocal,
        leaderElection,
        clock,
        boxStateLocal,
        registrationAccumulatorLocal,
        crossEpochForkLocal,
        proposalEventLocal,
        proposalConfig,
        appConfig.node.maxSupportedVersion
      )

      validatorsP2P <- Validators.make[F](
        cryptoResources,
        dataStores,
        bigBangBlockId,
        eligibilityCache,
        etaCalculation,
        consensusValidationStateP2P,
        leaderElection,
        clock,
        boxStateP2P,
        registrationAccumulatorP2P,
        crossEpochForkP2P,
        proposalEventLocal,
        proposalConfig,
        appConfig.node.maxSupportedVersion
      )

      protectedMempool <- MempoolProtected.make(
        mempool,
        validatorsP2P.transactionSemantics,
        validatorsP2P.transactionAuthorization,
        currentEventIdGetterSetters.canonicalHead.get().flatMap(dataStores.headers.getOrRaise),
        dataStores.transactions.getOrRaise,
        transactionRewardCalculator,
        costCalculator,
        box =>
          {
            for {
              blockId  <- OptionT(dataStores.txIdToBlockId.get(box.id))
              slotData <- OptionT(dataStores.slotData.get(blockId))
            } yield slotData.height
          }.value,
        appConfig.node.mempool.protection
      )

      localBlockchain = BlockchainCoreImpl(
        clock,
        dataStores,
        cryptoResources,
        blockIdTree,
        ConsensusImpl(
          validatorsLocal.header,
          validatorsLocal.headerToBody,
          chainSelection,
          consensusValidationStateLocal,
          etaCalculation,
          leaderElection,
          localChain
        ),
        LedgerImpl(
          validatorsLocal.transactionSyntax,
          validatorsLocal.transactionSemantics,
          validatorsLocal.transactionAuthorization,
          validatorsLocal.bodySyntax,
          validatorsLocal.bodySemantics,
          validatorsLocal.bodyAuthorization,
          protectedMempool,
          validatorsLocal.boxState,
          validatorsLocal.registrationAccumulator,
          validatorsLocal.rewardCalculator,
          costCalculator
        ),
        validatorsLocal,
        epochData,
        protocolConfig
      )
      p2pBlockchain = BlockchainCoreImpl(
        clock,
        dataStores,
        cryptoResources,
        blockIdTree,
        ConsensusImpl(
          validatorsP2P.header,
          validatorsP2P.headerToBody,
          chainSelection,
          consensusValidationStateP2P,
          etaCalculation,
          leaderElection,
          localChain
        ),
        LedgerImpl(
          validatorsP2P.transactionSyntax,
          validatorsP2P.transactionSemantics,
          validatorsP2P.transactionAuthorization,
          validatorsP2P.bodySyntax,
          validatorsP2P.bodySemantics,
          validatorsP2P.bodyAuthorization,
          protectedMempool,
          validatorsP2P.boxState,
          validatorsP2P.registrationAccumulator,
          validatorsP2P.rewardCalculator,
          costCalculator
        ),
        validatorsP2P,
        epochData,
        protocolConfig
      )

      regTestConfigOpt = appConfig.node.bigBang match {
        case p: ApplicationConfig.Node.BigBangs.Private => p.regtestConfig
        case _                                          => None
      }

      _ <- regTestConfigOpt.fold(ifEmpty = Logger[F].debug(show"Regtest is disabled").toResource) { config =>
        Logger[F].error(s"Regtest is enabled: with parameters $config").toResource
      }

      _ <- EthereumJsonRpc.serve(appConfig.node.ethereumJsonRpc.bindHost, appConfig.node.ethereumJsonRpc.bindPort)(
        new EthereumJsonRpcImpl(localBlockchain)
      )
      // Finally, run the program
      _ <- Blockchain
        .make[F](
          localBlockchain,
          p2pBlockchain,
          staking,
          eventSourcedStates,
          localPeer,
          p2pConfig.knownPeers,
          appConfig.node.rpc.bindHost,
          appConfig.node.rpc.bindPort,
          appConfig.node.rpc.networkControl,
          indexerServices ::: healthServices,
          (p2pConfig.publicHost, p2pConfig.publicPort).mapN(KnownPeer.apply),
          p2pConfig.networkProperties,
          appConfig.node.votedVersion,
          appConfig.node.votedProposal,
          regTestConfigOpt
        )
        .parProduct(indexerOpt.traverse(Replicator.background[F]).void)
        .parProduct(
          softwareVersion.traverse(VersionReplicator.background[F](_, appConfig.node.versionInfo.period)).void
        )
    } yield ()
  // scalastyle:on method.length

  private def makeLeaderElectionThreshold(
    blake2b512Resource:    Resource[F, Blake2b512],
    vrfConfig:             VrfConfig,
    slotGapLeaderElection: Long
  ) =
    for {
      exp   <- ExpInterpreter.make[F](10000, 38)
      log1p <- Log1pInterpreter.make[F](10000, 8).flatMap(Log1pInterpreter.makeCached[F])
      base = LeaderElectionValidation.make[F](vrfConfig, slotGapLeaderElection, blake2b512Resource, exp, log1p)
      leaderElectionThresholdCached <- LeaderElectionValidation.makeCached(base)
      leaderElectionThreshold = LeaderElectionValidation.makeWithCappedSlotDiff(
        leaderElectionThresholdCached,
        vrfConfig.lddCutoff
      )
    } yield leaderElectionThreshold

}
