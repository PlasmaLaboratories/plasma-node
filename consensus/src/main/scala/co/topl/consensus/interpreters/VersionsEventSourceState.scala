package co.topl.consensus.interpreters

import cats._
import cats.implicits._
import cats.effect.Async
import co.topl.algebras._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.box.Value.UpdateProposal
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockId
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.models._
import co.topl.node.models._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import co.topl.consensus.models.BlockHeader
import co.topl.proto.node.EpochData
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.crypto.hash.Blake2b256
import java.nio.ByteBuffer
import co.topl.consensus.algebras.VersionInfoAlgebra

case class ProposalConfig(
  proposalVotingMaxWindow: Int = 5,
  proposalVotingWindow:    Int = 2,
  // How many epochs shall pass before we could reuse proposal id
  proposalInactiveVotingWindow: Int = 1,
  updateProposalPercentage:     Double = 0.1,
  versionVotingWindow:          Int = 2,
  versionSwitchWindow:          Int = 2,
  updateVersionPercentage:      Double = 0.9 // Shall be more than 50%
)

object VersionsEventSourceState {

  case class VersionsData[F[_]](
    idToProposal: Store[F, ProposalId, UpdateProposal],

    // List of all proposal which are ACTIVE (not necessary created) during particular epoch
    epochToProposalIds: Store[F, Epoch, Set[ProposalId]],

    // Votes count for proposal,
    // if voting count is zero then proposal was active but no votes
    // if voting is not defined then proposal was not active
    proposalVoting: Store[F, (Epoch, ProposalId), Long],

    // Map of epoch when version has been CREATED to version id
    epochToCreatedVersionIds: Store[F, Epoch, Set[VersionId]],
    // List of all versions which are have at least one vote during particular epoch
    epochToVersionIds:   Store[F, Epoch, Set[VersionId]],
    versionIdToProposal: Store[F, VersionId, UpdateProposal],
    versionCounter:      Store[F, Unit, VersionId],
    versionVoting:       Store[F, (Epoch, VersionId), Long],
    // Describes from which era which version starts

    epochData: Store[F, Epoch, EpochData]
  )

  def getProposalId(proposal: UpdateProposal): Int =
    ByteBuffer.wrap(new Blake2b256().hash(proposal.toByteArray)).getInt

  private def getTargetEpoch(epoch: Epoch, config: ProposalConfig): Epoch =
    epoch + config.versionSwitchWindow

  def make[F[_]: Async: Logger](
    currentBlockId:      F[BlockId],
    parentChildTree:     ParentChildTree[F, BlockId],
    currentEventChanged: BlockId => F[Unit],
    initialState:        F[VersionsData[F]],
    clock:               ClockAlgebra[F],
    fetchHeader:         BlockId => F[BlockHeader],
    fetchBlockBody:      BlockId => F[BlockBody],
    fetchTransaction:    TransactionId => F[IoTransaction],
    versionAlgebra:      VersionInfoAlgebra[F],
    config:              ProposalConfig
  ): F[EventSourcedState[F, VersionsData[F], BlockId]] =
    EventSourcedState.OfTree.make(
      initialState = initialState,
      initialEventId = currentBlockId,
      applyEvent = new ApplyBlock(clock, fetchHeader, fetchBlockBody, fetchTransaction, versionAlgebra, config),
      unapplyEvent = new UnapplyBlock(clock, fetchHeader, fetchBlockBody, fetchTransaction, versionAlgebra, config),
      parentChildTree = parentChildTree,
      currentEventChanged
    )

  private class ApplyBlock[F[_]: MonadThrow: Logger](
    clock:            ClockAlgebra[F],
    fetchBlockHeader: BlockId => F[BlockHeader],
    fetchBlockBody:   BlockId => F[BlockBody],
    fetchTransaction: TransactionId => F[IoTransaction],
    versionAlgebra:   VersionInfoAlgebra[F],
    config:           ProposalConfig
  ) extends ((VersionsData[F], BlockId) => F[VersionsData[F]]) {

    import ApplyBlock._

    def apply(state: VersionsData[F], blockId: BlockId): F[VersionsData[F]] =
      for {
        header        <- fetchBlockHeader(blockId)
        currentEpoch  <- clock.epochOf(header.slot)
        previousEpoch <- clock.epochOf(header.parentSlot)
        _             <- Logger[F].info(show"Apply block $blockId of epoch $currentEpoch")

        _ <- if (currentEpoch != previousEpoch) epochBoundaryCrossed(state, previousEpoch, currentEpoch) else ().pure[F]
        _ <- applyNewProposals(state, blockId, currentEpoch)
        _ <- applyProposalVoting(state, header, currentEpoch) // we could create and vote for proposal in the same block
        _ <- applyVersionVoting(state, header, currentEpoch)
      } yield state

    private def applyNewProposals(state: VersionsData[F], blockId: BlockId, currentEpoch: Epoch): F[Unit] =
      for {
        blockTxs     <- fetchBlockBody(blockId)
        transactions <- blockTxs.transactionIds.traverse(fetchTransaction(_))
        proposals    <- transactions.flatMap(_.outputs).map(_.value.value).flatMap(_.updateProposal).toList.pure[F]
        _            <- proposals.traverse(proposal => applyNewProposal(state, currentEpoch, proposal))
      } yield ()

    private def applyNewProposal(
      state:          VersionsData[F],
      currentEpoch:   Epoch,
      updateProposal: UpdateProposal
    ): F[Unit] = {
      // Temporarily solution/stub id shall be defined in proposal itself
      val id = getProposalId(updateProposal)

      // Check that there is no active other proposal with the same id within proposalInactiveVotingWindow
      val window =
        Range.Long.inclusive(Math.max(0, currentEpoch - config.proposalInactiveVotingWindow), currentEpoch, 1)
      val inactiveWindowError = new IllegalStateException(show"Received proposal with id $id within $window")

      Logger[F].info(show"Received new proposal with id $id") >>
      Logger[F].info(show"Proposal id $id with data $updateProposal") >>
      window.toList.traverse(e => state.proposalVoting.get(e, id)).ensure(inactiveWindowError)(_.forall(_.isEmpty)) >>
      state.idToProposal.put(id, updateProposal) >>
      state.epochToProposalIds.addIdToEpoch(currentEpoch, id) >>
      state.proposalVoting.put((currentEpoch, id), 0)
    }

    private def applyProposalVoting(state: VersionsData[F], header: BlockHeader, currentEpoch: Epoch): F[Unit] =
      for {
        votedIdOpt <- header.getProposalVote.pure[F]
        _          <- votedIdOpt.traverse(votedId => state.proposalVoting.addOneVote((currentEpoch, votedId)))
      } yield ()

    private def applyVersionVoting(state: VersionsData[F], header: BlockHeader, currentEpoch: Epoch): F[Unit] =
      for {
        votedVersionOpt <- header.getVersionVote.pure[F]
        _ <- votedVersionOpt.traverse { version =>
          state.epochToVersionIds.addIdToEpoch(currentEpoch, version) >>
          state.versionVoting.createVoteAndAddOneVote((currentEpoch, version))
        }
      } yield ()

    private def epochBoundaryCrossed(state: VersionsData[F], previousEpoch: Epoch, currentEpoch: Epoch): F[Unit] =
      for {
        _             <- Logger[F].info(show"Crossing epoch $previousEpoch for apply block")
        prevProposals <- state.epochToProposalIds.get(previousEpoch)
        prevVersions  <- state.epochToVersionIds.get(previousEpoch)
        _ <- Logger[F].info(show"Epoch $previousEpoch state: proposals: $prevProposals; versions $prevVersions;")

        _ <- prevProposals match {
          case Some(proposalIds) => processPreviousEpochProposals(state, previousEpoch, currentEpoch, proposalIds)
          case None              => ().pure[F]
        }
        _ <- prevVersions match {
          case Some(versionIds) => processPreviousEpochVersions(state, previousEpoch, currentEpoch, versionIds)
          case None             => ().pure[F]
        }
      } yield ()

    private def processPreviousEpochProposals(
      state:         VersionsData[F],
      previousEpoch: Epoch,
      currentEpoch:  Epoch,
      proposalIds:   Set[ProposalId]
    ): F[Unit] =
      for {
        votingMaxWindow <- getVotingRangeForMaxWindow(previousEpoch).pure[F]
        blocksInEpochs  <- getEpochsBlockCounts(state, votingMaxWindow)
        // We MUST sort proposals for to have the same proposal processing orders
        proposalResults <- proposalIds.toList.sorted.traverse(getProposalResult(_, state, blocksInEpochs))
        proposalActions <- proposalResults.map { case (id, votes) => id -> getResultForProposal(votes) }.pure[F]
        _               <- proposalActions.traverse { case (id, act) => doProposalAction(id, act, currentEpoch, state) }
      } yield ()

    private def getVotingRangeForMaxWindow(epoch: Epoch) =
      Range.Long.inclusive(Math.max(0, epoch - config.proposalVotingMaxWindow), epoch, 1)

    private def getEpochsBlockCounts(state: VersionsData[F], votingRange: Seq[Epoch]): F[Seq[(Epoch, Long)]] =
      votingRange
        .traverse(e => state.epochData.getOrRaise(e).map(d => e -> (1 + d.endHeight - d.startHeight)))

    // return voting result for proposal for "block count" epochs
    // Some(true) -- enough voting for proposal
    // Some(false) -- not enough voting for proposal
    // None -- No voting was active for epoch
    private def getProposalResult(
      proposal:    ProposalId,
      state:       VersionsData[F],
      blocksCount: Seq[(Epoch, Long)]
    ): F[(ProposalId, Seq[Option[Boolean]])] =
      blocksCount
        .traverse { case (e, blockCount) =>
          state.proposalVoting
            .get((e, proposal))
            .map(_.map(_ / blockCount.toDouble))
            .flatTap(p => Logger[F].info(show"Voting result for proposal $proposal epoch $e: $p"))
            .map(_.map(_ >= config.updateProposalPercentage))
        // .map(_.map(_ / blockCount.toDouble >= config.updateProposalPercentage))
        }
        .map(percentageVotes => proposal -> percentageVotes)

    private def getResultForProposal(votes: Seq[Option[Boolean]]): ProposalAction =
      if (
        votes.sizeIs >= config.proposalVotingWindow &&
        votes.takeRight(config.proposalVotingWindow).forall(_.getOrElse(false))
      ) {
        ProposalAction.ToVersion
      } else if (
        votes.sizeIs >= config.proposalVotingMaxWindow &&
        votes.forall(_.isDefined)
      ) {
        ProposalAction.Delete
      } else {
        ProposalAction.Keep
      }

    private def doProposalAction(
      proposalId:   ProposalId,
      result:       ProposalAction,
      currentEpoch: Epoch,
      state:        VersionsData[F]
    ): F[Unit] =
      result match {
        case ProposalAction.Delete => // we will not keep proposal then
          Logger[F].info(show"Proposal $proposalId had been expired, remove it from proposal list")
        case ProposalAction.Keep =>
          Logger[F].info(show"Proposal $proposalId had not been expired, keep it in proposal list") >>
          state.epochToProposalIds.addIdToEpoch(currentEpoch, proposalId) >>
          state.proposalVoting.put((currentEpoch, proposalId), 0)
        case ProposalAction.ToVersion =>
          Logger[F].info(show"Proposal $proposalId pass voting, new version shall be created") >>
          state.idToProposal.getOrRaise(proposalId).flatMap(p => proposalToVersion(state, currentEpoch, proposalId, p))
      }

    private def proposalToVersion(
      state:        VersionsData[F],
      currentEpoch: Epoch,
      proposalId:   ProposalId,
      proposal:     UpdateProposal
    ): F[Unit] =
      for {
        versionId <- state.versionCounter.getFreeVersion()
        _         <- state.epochToCreatedVersionIds.addIdToEpoch(currentEpoch, versionId)
        _         <- state.versionIdToProposal.put(versionId, proposal)
        _         <- Logger[F].info(show"Created new version $versionId from proposal $proposalId")
      } yield ()

    private def processPreviousEpochVersions(
      state:         VersionsData[F],
      previousEpoch: Epoch,
      currentEpoch:  Epoch,
      versionIds:    Set[VersionId]
    ): F[Unit] =
      for {
        votingWindow   <- getVersionVotingEpochs(previousEpoch).pure[F]
        blocksInEpochs <- getEpochsBlockCounts(state, votingWindow)
        versionResults <- versionIds.toList.sorted.traverse(getVersionResult(_, state, blocksInEpochs))
        versionActions <- versionResults.map { case (id, votes) => id -> getResultForVersion(votes) }.pure[F]
        newVersions    <- versionActions.collect { case (version, VersionAction.toActive) => version }.pure[F]
        _ <- newVersions.ensuring(_.sizeIs < 2, show"More than one version became active: $newVersions").pure[F]
        targetEpoch <- getTargetEpoch(currentEpoch, config).pure[F]
        _           <- newVersions.traverse(doVersionAction(targetEpoch, _))
      } yield ()

    private def getVersionVotingEpochs(epoch: Epoch) =
      Range.Long.inclusive(Math.max(0, epoch - config.versionVotingWindow), epoch, 1)

    // return voting result for version for "block count" epochs
    // true -- enough voting for version
    // false -- not enough voting for version
    private def getVersionResult(
      version:     VersionId,
      state:       VersionsData[F],
      blocksCount: Seq[(Epoch, Long)]
    ): F[(VersionId, Seq[Boolean])] =
      blocksCount
        .traverse { case (e, blockCount) =>
          state.versionVoting
            .get((e, version))
            .map(_.getOrElse(0L) / blockCount.toDouble)
            .flatTap(p => Logger[F].info(show"Voting result for version $version epoch $e: $p"))
            .map(_ >= config.updateVersionPercentage)
        }
        .map(percentageVotes => version -> percentageVotes)

    private def getResultForVersion(votes: Seq[Boolean]): VersionAction =
      if (
        votes.sizeIs >= config.versionVotingWindow &&
        votes.takeRight(config.versionVotingWindow).forall(identity)
      ) {
        VersionAction.toActive
      } else VersionAction.Keep

    private def doVersionAction(targetEpoch: Epoch, version: VersionId): F[Unit] =
      for {
        _ <- Logger[F].info(show"Version $version passed voting. New version will be active from $targetEpoch epoch")
        _ <- versionAlgebra.addVersionStartEpoch(targetEpoch, version)
      } yield ()
  }

  private object ApplyBlock {
    sealed trait ProposalAction

    private object ProposalAction {
      case object Delete extends ProposalAction
      case object Keep extends ProposalAction
      case object ToVersion extends ProposalAction
    }

    sealed trait VersionAction

    private object VersionAction {
      case object Keep extends VersionAction
      case object toActive extends VersionAction
    }
  }

  private class UnapplyBlock[F[_]: MonadThrow: Logger](
    clock:            ClockAlgebra[F],
    fetchBlockHeader: BlockId => F[BlockHeader],
    fetchBlockBody:   BlockId => F[BlockBody],
    fetchTransaction: TransactionId => F[IoTransaction],
    versionAlgebra:   VersionInfoAlgebra[F],
    config:           ProposalConfig
  ) extends ((VersionsData[F], BlockId) => F[VersionsData[F]]) {

    def apply(state: VersionsData[F], blockId: BlockId): F[VersionsData[F]] =
      for {
        header        <- fetchBlockHeader(blockId)
        currentEpoch  <- clock.epochOf(header.slot)
        previousEpoch <- clock.epochOf(header.parentSlot)
        _             <- Logger[F].info(show"Unapply block $blockId of epoch $currentEpoch")
        _             <- unapplyProposalVoting(state, header, currentEpoch)
        _             <- unapplyVersionVoting(state, header, currentEpoch)
        _             <- unapplyNewProposals(state, blockId, currentEpoch)
        _             <- if (currentEpoch != previousEpoch) epochBoundaryCrossed(state, currentEpoch) else ().pure[F]
      } yield state

    private def epochBoundaryCrossed(state: VersionsData[F], currentEpoch: Epoch): F[Unit] =
      for {
        _            <- Logger[F].info(show"Crossing epoch $currentEpoch for unapply block")
        proposalsOpt <- state.epochToProposalIds.get(currentEpoch)
        _            <- proposalsOpt.fold(().pure[F])(clearProposalsVoting(state, _, currentEpoch))
        _            <- state.epochToProposalIds.remove(currentEpoch)

        votedVersionsOpt <- state.epochToVersionIds.get(currentEpoch)
        _                <- votedVersionsOpt.fold(().pure[F])(clearVotedVersions(state, currentEpoch, _))

        createdVersionsOpt <- state.epochToCreatedVersionIds.get(currentEpoch)
        _                  <- createdVersionsOpt.fold(().pure[F])(clearCreatedVersions(state, _))
        _                  <- state.epochToCreatedVersionIds.remove(currentEpoch)

        _ <- versionAlgebra.removeVersionStartEpoch(getTargetEpoch(currentEpoch, config))
      } yield ()

    private def clearProposalsVoting(
      state:        VersionsData[F],
      proposalIds:  Set[ProposalId],
      currentEpoch: Epoch
    ): F[Unit] =
      Logger[F].info(show"Going to unapply proposals $proposalIds from epoch $currentEpoch") >>
      proposalIds.toList.traverse(id => state.proposalVoting.deleteVoting(currentEpoch, id)).void

    private def clearVotedVersions(
      state:        VersionsData[F],
      currentEpoch: Epoch,
      versionIds:   Set[ProposalId]
    ): F[Unit] = versionIds.toList
      .traverse(_ => state.epochToVersionIds.remove(currentEpoch))
      .void

    private def clearCreatedVersions(
      state:      VersionsData[F],
      versionIds: Set[ProposalId]
    ): F[Unit] = versionIds.toList
      .traverse(version =>
        state.versionCounter.decrementVersion >>
        state.versionIdToProposal.remove(version)
      )
      .void

    private def unapplyProposalVoting(state: VersionsData[F], header: BlockHeader, currentEpoch: Epoch): F[Unit] =
      for {
        votedIdOpt <- header.getProposalVote.pure[F]
        _          <- votedIdOpt.traverse(votedId => state.proposalVoting.removeOneVote((currentEpoch, votedId)))
      } yield ()

    private def unapplyVersionVoting(state: VersionsData[F], header: BlockHeader, currentEpoch: Epoch): F[Unit] =
      for {
        votedVersionOpt <- header.getVersionVote.pure[F]
        _               <- votedVersionOpt.traverse(vote => state.versionVoting.removeOneVote((currentEpoch, vote)))
      } yield ()

    private def unapplyNewProposals(state: VersionsData[F], blockId: BlockId, currentEpoch: Epoch): F[Unit] =
      for {
        blockTxs     <- fetchBlockBody(blockId)
        transactions <- blockTxs.transactionIds.traverse(fetchTransaction(_))
        proposals    <- transactions.flatMap(_.outputs).map(_.value.value).flatMap(_.updateProposal).toList.pure[F]
        idToProposalUpdate <- proposals.map(p => getProposalId(p) -> p).pure[F]
        _ <- idToProposalUpdate.traverse { case (id, _) =>
          Logger[F].info(show"Remove proposal with id $id") >>
          state.idToProposal.remove(id) >>
          state.epochToProposalIds.removeIdFromEpoch(currentEpoch, id) >>
          state.proposalVoting.deleteVoting((currentEpoch, id))
        }
      } yield ()

  }

  implicit class epochToTOps[F[_]: MonadThrow, T](storage: Store[F, Epoch, Set[T]]) {

    def addIdToEpoch(epoch: Epoch, id: T): F[Unit] =
      for {
        currentIds <- storage.get(epoch)
        _          <- storage.put(epoch, currentIds.fold(Set(id))(_ + id))
      } yield ()

    def removeIdFromEpoch(epoch: Epoch, id: T): F[Unit] =
      for {
        currentIds <- storage.get(epoch)
        _          <- storage.put(epoch, currentIds.fold(Set.empty[T])(_ - id))
      } yield ()
  }

  implicit class proposalVotingOps[F[_]: MonadThrow: Logger](storage: Store[F, (Epoch, ProposalId), Long]) {

    def addOneVote(vote: (Epoch, ProposalId)): F[Unit] =
      for {
        currentVote <- storage.getOrRaise(vote)
        _           <- storage.put(vote, currentVote + 1)
      } yield ()

    def createVoteAndAddOneVote(vote: (Epoch, ProposalId)): F[Unit] =
      for {
        currentVote <- storage.get(vote).flatMap {
          case Some(actualVote) => actualVote.pure[F]
          case None             => Logger[F].info(show"Create vote for $vote") >> 0L.pure[F]
        }
        _ <- storage.put(vote, currentVote + 1)
      } yield ()

    def removeOneVote(vote: (Epoch, ProposalId)): F[Unit] =
      for {
        currentVote <- storage.getOrRaise(vote).ensure(new IllegalStateException("Negative vote counter"))(_ > 0)
        _           <- storage.put(vote, currentVote - 1)
      } yield ()

    def deleteVoting(vote: (Epoch, ProposalId)): F[Unit] =
      for {
        _ <- storage.getOrRaise(vote).ensure(new IllegalStateException("Try remove non negative vote"))(_ == 0)
        _ <- storage.remove(vote)
      } yield ()
  }

  implicit class VersionCounterOps[F[_]: MonadThrow](storage: Store[F, Unit, VersionId]) {

    def decrementVersion: F[Unit] =
      for {
        currentVersion <- storage.getOrRaise(())
        _              <- storage.put((), currentVersion - 1)
      } yield ()

    def getFreeVersion(): F[Int] =
      for {
        currentFreeVersion <- storage.getOrRaise(())
        _                  <- storage.put((), currentFreeVersion + 1)
      } yield currentFreeVersion
  }

  implicit class BlockHeaderVersionsOps(header: BlockHeader) {

    def getProposalVote: Option[ProposalId] =
      Option.when(header.version.thirdDigit != emptyProposal)(header.version.thirdDigit)

    def getVersionVote: Option[ProposalId] =
      Option.when(header.version.secondDigit != emptyVersion)(header.version.secondDigit)
  }

  implicit val updateProposalShow: Show[UpdateProposal] = proposal => proposal.toString
}
