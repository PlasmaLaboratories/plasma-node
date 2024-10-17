package org.plasmalabs.consensus.interpreters

import cats.effect.IO
import cats.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.plasmalabs.algebras.ClockAlgebra
import org.plasmalabs.algebras.testInterpreters._
import org.plasmalabs.consensus.interpreters.VotingEventSourceState.VotingData
import org.plasmalabs.consensus.models.BlockHeaderValidationFailures._
import org.plasmalabs.consensus.models.{BlockHeaderValidationFailure, _}
import org.plasmalabs.eventtree.EventSourcedState
import org.plasmalabs.models.ModelGenerators.GenHelper
import org.plasmalabs.models._
import org.plasmalabs.models.generators.consensus.ModelGenerators._
import org.plasmalabs.sdk.models.box.Value.ConfigProposal

import scala.collection.immutable.NumericRange
import scala.concurrent.duration.FiniteDuration

class BlockHeaderVotingValidationTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
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
    nextFreeVersion:    VersionId = 0,
    availableProposals: Seq[(Epoch, ProposalId)] = Seq.empty
  ): F[EventSourcedState[F, VotingData[F], BlockId]] =
    for {
      epochToProposalIds       <- TestStore.make[F, Epoch, Set[ProposalId]]
      proposalVoting           <- TestStore.make[F, (Epoch, ProposalId), Long]
      _                        <- availableProposals.traverse(d => proposalVoting.put(d, 0))
      epochToCreatedVersionIds <- TestStore.make[F, Epoch, Set[VersionId]]
      epochToVersionIds        <- TestStore.make[F, Epoch, Set[VersionId]]
      versionIdToProposal      <- TestStore.make[F, VersionId, ConfigProposal]
      versionCounter           <- TestStore.make[F, Unit, VersionId]
      _                        <- versionCounter.put((), nextFreeVersion)
      versionVoting            <- TestStore.make[F, (Epoch, VersionId), Long]
      versionInfoStore         <- TestStore.make[F, Epoch, VersionId]
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

  private def headerWithVersionVote(version: VersionId = 0) =
    arbitraryHeader.arbitrary.first.copy(version = ProtocolVersion(secondDigit = version))

  test("Version voting shall use only available version") {
    withMock {
      val nextFreeVersion = 10
      for {
        eventSource      <- makeVotingEventSource(nextFreeVersion = nextFreeVersion)
        votingValidation <- BlockHeaderVotingValidation.make(defaultClocks, eventSource)
        block1 = headerWithVersionVote(nextFreeVersion - 1)
        res1 <- votingValidation.validate(block1)
        _    <- assert(res1.isRight).pure[F]

        block2 = headerWithVersionVote(nextFreeVersion)
        res2 <- votingValidation.validate(block2)
        _    <- assert(res2 == Left(IncorrectVotedVersionId(nextFreeVersion))).pure[F]

        block3 = headerWithVersionVote(nextFreeVersion + 1)
        res3 <- votingValidation.validate(block3)
        _    <- assert(res3 == Left(IncorrectVotedVersionId(nextFreeVersion + 1))).pure[F]
      } yield ()
    }
  }

  private def headerOfEpochWithProposalVote(epoch: Epoch = 0, proposalId: ProposalId) =
    arbitraryHeader.arbitrary.first.copy(slot = epoch + 1, version = ProtocolVersion(thirdDigit = proposalId))

  test("Proposal vote shall use only available proposals") {
    withMock {
      for {
        eventSource      <- makeVotingEventSource(availableProposals = Seq((1, 1), (2, 2)))
        votingValidation <- BlockHeaderVotingValidation.make(defaultClocks, eventSource)
        block1 = headerOfEpochWithProposalVote(1, 1)
        res1 <- votingValidation.validate(block1)
        _    <- assert(res1.isRight).pure[F]

        block2 = headerOfEpochWithProposalVote(2, 1)
        res2 <- votingValidation.validate(block2)
        _    <- assert(res2 == Left(IncorrectVotedProposalId(1))).pure[F]

        block3 = headerOfEpochWithProposalVote(1, 2)
        res3 <- votingValidation.validate(block3)
        _    <- assert(res3 == Left(IncorrectVotedProposalId(2))).pure[F]
      } yield ()
    }
  }

  test("Event source use parent of block to get correct state") {
    withMock {
      val block = arbitraryHeader.arbitrary.first
      val eventSource = mock[EventSourcedState[F, VotingData[F], BlockId]]
      (eventSource
        .useStateAt(_: BlockId)(_: VotingData[F] => F[Either[BlockHeaderValidationFailure, BlockHeader]]))
        .expects(block.parentHeaderId, *)
        .once()
        .returns(Either.right[BlockHeaderValidationFailure, BlockHeader](block).pure[F])
      for {
        votingValidation <- BlockHeaderVotingValidation.make(defaultClocks, eventSource)
        res1             <- votingValidation.validate(block)
        _                <- assert(res1 == Either.right[BlockHeaderValidationFailure, BlockHeader](block)).pure[F]
      } yield ()
    }
  }
}
