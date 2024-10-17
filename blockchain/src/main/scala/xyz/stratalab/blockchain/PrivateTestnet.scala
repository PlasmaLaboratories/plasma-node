package org.plasmalabs.blockchain

import cats.effect.Async
import cats.implicits._
import com.google.protobuf.ByteString
import fs2.Chunk
import fs2.io.file.{Files, Path}
import org.typelevel.log4cats.Logger
import quivr.models.{Int128, Proposition}
import org.plasmalabs.blockchain.BigBang.Config
import org.plasmalabs.codecs.bytes.tetra.instances.persistableKesProductSecretKey
import org.plasmalabs.codecs.bytes.typeclasses.Persistable
import org.plasmalabs.config.ApplicationConfig
import org.plasmalabs.consensus.models.ProtocolVersion
import org.plasmalabs.crypto.hash.Blake2b256
import org.plasmalabs.crypto.models.SecretKeyKesProduct
import org.plasmalabs.models._
import org.plasmalabs.models.utility._
import org.plasmalabs.numerics.implicits._
import org.plasmalabs.sdk.constants.NetworkConstants
import org.plasmalabs.sdk.models.box.{Challenge, Lock, Value}
import org.plasmalabs.sdk.models.transaction.{IoTransaction, UnspentTransactionOutput}
import org.plasmalabs.sdk.models.{Datum, LockAddress}
import org.plasmalabs.sdk.syntax._

import scala.concurrent.duration._

object PrivateTestnet {

  val DefaultTotalStake: Int128 = 10_000_000L
  val DefaultTotalLvls: Int128 = 10_000_000L

  /**
   * Constructs several Operator StakerInitializers.  A Staker is initialized using the concatenation of the timestamp (bytes)
   * with the staker's index (bytes).
   * @param timestamp The Big Bang Timestamp (used as the "seed" of the blockchain)
   * @param stakerCount the number of stakers to initialize.  0 is technically accepted and will return an empty list,
   *                    but the subsequent blockchain will be practically useless.  Without any initial operators,
   *                    the 2nd block of the chain can never be produced
   */
  def stakerInitializers(timestamp: Timestamp, stakerCount: Int): List[StakerInitializers.Operator] = {
    require(stakerCount >= 0)
    val blake2b256 = new Blake2b256()
    List
      .tabulate(stakerCount)(index =>
        // This staker's "seed" is concatenation of timestamp bytes + index bytes
        blake2b256.hash(
          ByteString.copyFrom(BigInt(timestamp).toByteArray).concat(ByteString.copyFrom(BigInt(index).toByteArray))
        )
      )
      .map(bytes => StakerInitializers.Operator(bytes, (9, 9), HeightLockOneSpendingAddress))
  }

  def defaultStake(stakerCount: Int): BigInt = Ratio(DefaultTotalStake, stakerCount).round

  /**
   * Constructs a BigBang Config containing registrations of the given Stakers.  In addition, a single Poly box is
   * produced and is publicly spendable.
   */
  def config(
    timestamp:       Timestamp,
    stakers:         List[StakerInitializers.Operator],
    stakes:          Option[List[BigInt]],
    protocolVersion: ProtocolVersion,
    protocol:        ApplicationConfig.Node.Protocol
  ): BigBang.Config = {
    require(stakes.forall(_.sizeIs == stakers.length), "stakes must be the same length as stakers")
    val transactions =
      stakers
        .zip(stakes.getOrElse(List.fill(stakers.length)(defaultStake(stakers.length))))
        .map { case (staker, stake) => staker.registrationTransaction(stake) }
        .appended(
          IoTransaction(
            inputs = Nil,
            outputs = List(
              UnspentTransactionOutput(
                HeightLockOneSpendingAddress,
                Value.defaultInstance.withLvl(Value.LVL(DefaultTotalLvls))
              ),
              UnspentTransactionOutput(
                HeightLockOneSpendingAddress,
                Value.defaultInstance.withConfigProposal(BigBang.protocolToConfigProposal(protocol))
              )
            ),
            datum = Datum.IoTransaction.defaultInstance
          )
        )
        .map(_.embedId)
    BigBang.Config(
      timestamp,
      transactions,
      Config.DefaultEtaPrefix,
      protocolVersion
    )
  }

  def writeStaker[F[_]: Async: Logger](
    stakingDir:  Path,
    initializer: StakerInitializers.Operator,
    stake:       Int128
  ): F[Unit] =
    Files.forAsync[F].createDirectories(stakingDir) >>
    Files
      .forAsync[F]
      .list(stakingDir)
      .compile
      .count
      .flatMap(c =>
        if (c > 0) Logger[F].info(show"Staker already initialized at $stakingDir")
        else
          for {
            _ <- Logger[F].info(show"Generating a private testnet genesis staker into $stakingDir")
            registrationTx = initializer.registrationTransaction(stake)
            writeFile = (name: String, data: Array[Byte]) =>
              fs2.Stream
                .chunk(Chunk.array(data))
                .through(Files.forAsync[F].writeAll(stakingDir / name))
                .compile
                .drain
            _ <- Files.forAsync[F].createDirectories(stakingDir / StakingInit.KesDirectoryName)
            _ <- writeFile(
              s"${StakingInit.KesDirectoryName}/0",
              Persistable[SecretKeyKesProduct].persistedBytes(initializer.kesSK).toByteArray
            )
            _ <- writeFile(StakingInit.VrfKeyName, initializer.vrfSK.toByteArray)
            _ <- writeFile(StakingInit.RegistrationTxName, registrationTx.toByteArray)
          } yield ()
      )

  private val HeightLockOneProposition: Proposition =
    Proposition(
      Proposition.Value.HeightRange(
        Proposition.HeightRange("header", 1, Long.MaxValue)
      )
    )

  private val HeightLockOneChallenge: Challenge =
    Challenge().withRevealed(HeightLockOneProposition)

  val HeightLockOneLock: Lock =
    Lock(
      Lock.Value.Predicate(
        Lock.Predicate(
          List(HeightLockOneChallenge),
          1
        )
      )
    )

  val HeightLockOneSpendingAddress: LockAddress = HeightLockOneLock.lockAddress(
    NetworkConstants.PRIVATE_NETWORK_ID,
    NetworkConstants.MAIN_LEDGER_ID
  )

  val DefaultProtocolVersion: ProtocolVersion = ProtocolVersion(2, 0, 0)

  val DefaultProtocol: ApplicationConfig.Node.Protocol =
    ApplicationConfig.Node.Protocol(
      minAppVersion = "2.0.0",
      fEffective = Ratio(12, 100),
      vrfLddCutoff = 15,
      vrfPrecision = 40,
      vrfBaselineDifficulty = Ratio(5, 100),
      vrfAmplitude = Ratio(50, 100),
      slotGapLeaderElection = 0,
      chainSelectionKLookback = 5184,
      slotDuration = 1.seconds,
      forwardBiasedSlotWindow = 50,
      operationalPeriodsPerEpoch = 25,
      kesKeyHours = 9,
      kesKeyMinutes = 9,
      epochLengthOverride = None
    )

}
