package org.plasmalabs.consensus.interpreters

import cats.effect.IO
import cats.implicits.*
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.algebras.testInterpreters.*
import org.plasmalabs.algebras.{ClockAlgebra, Store}
import org.plasmalabs.consensus.interpreters.CrossEpochEventSourceState.VotingData
import org.plasmalabs.consensus.models.*
import org.plasmalabs.consensus.models.BlockHeaderValidationFailures.*
import org.plasmalabs.eventtree.EventSourcedState
import org.plasmalabs.models.*
import org.plasmalabs.models.ModelGenerators.GenHelper
import org.plasmalabs.models.generators.consensus.ModelGenerators.*
import org.plasmalabs.sdk.models.box.Value.ConfigProposal
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.immutable.NumericRange
import scala.concurrent.duration.FiniteDuration

class BlockHeaderVersionValidationTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  type F[A] = IO[A]

  private val defaultClocks = new ClockAlgebra[F] {
    override def slotLength: F[FiniteDuration] = ???
    override def slotsPerEpoch: F[Long] = 1L.pure[F]
    override def slotsPerOperationalPeriod: F[Long] = ???
    override def currentEpoch: F[Epoch] = ???
    override def globalSlot: F[Slot] = ???
    override def currentTimestamp: F[Timestamp] = ???
    override def forwardBiasedSlotWindow: F[Slot] = ???
    override def timestampToSlot(timestamp:       Timestamp): F[Slot] = ???
    override def slotToTimestamps(slot:           Slot): F[NumericRange.Inclusive[Timestamp]] = ???
    override def delayedUntilSlot(slot:           Slot): F[Unit] = ???
    override def delayedUntilTimestamp(timestamp: Timestamp): F[Unit] = ???
  }

  private def makeVotingEventSource(
    versionInfoStore: Store[F, Epoch, VersionId]
  ): F[EventSourcedState[F, VotingData[F], BlockId]] =
    for {
      epochToProposalIds       <- TestStore.make[F, Epoch, Set[ProposalId]]
      proposalVoting           <- TestStore.make[F, (Epoch, ProposalId), Long]
      epochToCreatedVersionIds <- TestStore.make[F, Epoch, Set[VersionId]]
      epochToVersionIds        <- TestStore.make[F, Epoch, Set[VersionId]]
      versionIdToProposal      <- TestStore.make[F, VersionId, ConfigProposal]
      versionCounter           <- TestStore.make[F, Unit, VersionId]
      versionVoting            <- TestStore.make[F, (Epoch, VersionId), Long]
      versionInfoAlgebra       <- VersionInfo.make[F](versionInfoStore)
      votingData = VotingData[F](
        epochToProposalIds,
        proposalVoting,
        epochToCreatedVersionIds,
        epochToVersionIds,
        versionIdToProposal,
        versionCounter,
        versionVoting,
        versionInfoAlgebra
      )
    } yield new EventSourcedState[F, VotingData[F], BlockId] {
      override def stateAt(eventId:       BlockId): F[VotingData[F]] = ???
      override def useStateAt[U](eventId: BlockId)(f: VotingData[F] => F[U]): F[U] = f(votingData)
    }

  private def headerInEpochWithVersion(epoch: Epoch, version: VersionId = 0) =
    arbitraryHeader.arbitrary.first.copy(slot = epoch + 1, version = ProtocolVersion(versionId = version))

  test("Header version validation shall be done according to version information") {
    withMock {
      val maxVersion = 1000
      for {
        versionStore      <- TestStore.make[F, Epoch, VersionId]
        _                 <- versionStore.put(Long.MinValue, -1)
        _                 <- versionStore.put(0, 1)
        _                 <- versionStore.put(10, 2)
        votingState       <- makeVotingEventSource(versionStore)
        versionValidation <- BlockHeaderVersionValidation.make(defaultClocks, votingState, maxVersion)
        block1 = headerInEpochWithVersion(-1)
        res1 <- versionValidation.validate(block1)
        _    <- assert(res1 == Left(IncorrectVersionId(-1, 0))).pure[F]

        block2 = headerInEpochWithVersion(0)
        res2 <- versionValidation.validate(block2)
        _    <- assert(res2 == Left(IncorrectVersionId(1, 0))).pure[F]

        block3 = headerInEpochWithVersion(1)
        res3 <- versionValidation.validate(block3)
        _    <- assert(res3 == Left(IncorrectVersionId(1, 0))).pure[F]

        block4 = headerInEpochWithVersion(10)
        res4 <- versionValidation.validate(block4)
        _    <- assert(res4 == Left(IncorrectVersionId(2, 0))).pure[F]

        block5 = headerInEpochWithVersion(23)
        res5 <- versionValidation.validate(block5)
        _    <- assert(res5 == Left(IncorrectVersionId(2, 0))).pure[F]

        block6 = headerInEpochWithVersion(10, 2)
        res6 <- versionValidation.validate(block6)
        _    <- assert(res6 == Right(block6)).pure[F]

        block7 = headerInEpochWithVersion(10, maxVersion)
        res7 <- versionValidation.validate(block7)
        _    <- assert(res7 == Left(IncorrectVersionId(2, 1000))).pure[F] // but supported check pass

        block8 = headerInEpochWithVersion(10, maxVersion + 1)
        res8 <- versionValidation.validate(block8)
        _    <- assert(res8 == Left(UnsupportedVersionId(maxVersion + 1, maxVersion))).pure[F]
      } yield ()
    }
  }

}
