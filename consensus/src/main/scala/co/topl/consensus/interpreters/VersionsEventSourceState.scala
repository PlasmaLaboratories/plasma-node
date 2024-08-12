package co.topl.consensus.interpreters

import cats._
import cats.implicits._
import cats.effect.Async
import co.topl.algebras.{Store, _}
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.box.Value.UpdateProposal
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockId
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.models.{Epoch, _}
import co.topl.node.models._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import co.topl.consensus.models.BlockHeader
import co.topl.proto.node.EpochData
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.crypto.hash.Blake2b256
import java.nio.ByteBuffer

case class ProposalConfig(
  proposalVotingMaxWindow: Int = 5,
  proposalVotingWindow:    Int = 2,
  // How many epochs shall pass before we could reuse proposal id
  proposalInactiveVotingWindow: Int = 1,
  updateProposalPercentage:     Double = 0.1
)

object VersionsEventSourceState {

  case class VersionsData[F[_]](
    idToProposal: Store[F, ProposalId, UpdateProposal],

    // List of all proposal which are ACTIVE (not created) during particular epoch
    epochToProposalIds: Store[F, Epoch, Set[ProposalId]],

    // Votes count for proposal,
    // if voting count is zero then proposal was active but no votes
    // if voting is not defined then proposal was not active
    proposalVoting: Store[F, (Epoch, ProposalId), Long],

    // Map of epoch when version has been CREATED to version id
    epochToVersionIds:   Store[F, Epoch, Set[VersionId]],
    versionIdToProposal: Store[F, VersionId, UpdateProposal],
    versionCounter:      Store[F, Unit, VersionId],
    epochData:           Store[F, Epoch, EpochData]
  )

  def getProposalId(proposal: UpdateProposal): Int =
    ByteBuffer.wrap(new Blake2b256().hash(proposal.toByteArray)).getInt

  def make[F[_]: Async: Logger](
    currentBlockId:      F[BlockId],
    parentChildTree:     ParentChildTree[F, BlockId],
    currentEventChanged: BlockId => F[Unit],
    initialState:        F[VersionsData[F]],
    clock:               ClockAlgebra[F],
    fetchBlockHeader:    BlockId => F[BlockHeader],
    fetchBlockBody:      BlockId => F[BlockBody],
    fetchTransaction:    TransactionId => F[IoTransaction],
    config:              ProposalConfig
  ): F[EventSourcedState[F, VersionsData[F], BlockId]] =
    EventSourcedState.OfTree.make(
      initialState = initialState,
      initialEventId = currentBlockId,
      applyEvent = new ApplyBlock(clock, fetchBlockHeader, fetchBlockBody, fetchTransaction, config),
      unapplyEvent = new UnapplyBlock(clock, fetchBlockHeader, fetchBlockBody, fetchTransaction),
      parentChildTree = parentChildTree,
      currentEventChanged
    )

  private class ApplyBlock[F[_]: MonadThrow: Logger](
    clock:            ClockAlgebra[F],
    fetchBlockHeader: BlockId => F[BlockHeader],
    fetchBlockBody:   BlockId => F[BlockBody],
    fetchTransaction: TransactionId => F[IoTransaction],
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
        _ <- applyVoting(state, header, currentEpoch) // we could vote for proposal in the same block

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

    private def applyVoting(state: VersionsData[F], header: BlockHeader, currentEpoch: Epoch): F[Unit] =
      for {
        votedIdOpt <- header.getProposalVote.pure[F]
        _          <- votedIdOpt.traverse(votedId => state.proposalVoting.addOneVote((currentEpoch, votedId)))
      } yield ()

    private def epochBoundaryCrossed(state: VersionsData[F], previousEpoch: Epoch, currentEpoch: Epoch): F[Unit] =
      Logger[F].info(show"Crossing epoch $previousEpoch for apply block") >>
      state.epochToProposalIds.get(previousEpoch).flatMap {
        case Some(proposalIds) => processPreviousEpochProposals(state, previousEpoch, currentEpoch, proposalIds)
        case None              => ().pure[F]
      }

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
        .traverse(e => state.epochData.getOrRaise(e).map(d => e -> (d.endHeight - d.startHeight)))

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
            .map(_.map(_ / blockCount.toDouble >= config.updateProposalPercentage))
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
        _         <- state.epochToVersionIds.addIdToEpoch(currentEpoch, versionId)
        _         <- state.versionIdToProposal.put(versionId, proposal)
        _         <- Logger[F].info(show"Created new version $versionId from proposal $proposalId")
      } yield ()
  }

  private object ApplyBlock {
    sealed trait ProposalAction

    private object ProposalAction {
      case object Delete extends ProposalAction
      case object Keep extends ProposalAction
      case object ToVersion extends ProposalAction
    }
  }

  private class UnapplyBlock[F[_]: MonadThrow: Logger](
    clock:            ClockAlgebra[F],
    fetchBlockHeader: BlockId => F[BlockHeader],
    fetchBlockBody:   BlockId => F[BlockBody],
    fetchTransaction: TransactionId => F[IoTransaction]
  ) extends ((VersionsData[F], BlockId) => F[VersionsData[F]]) {

    def apply(state: VersionsData[F], blockId: BlockId): F[VersionsData[F]] =
      for {
        header        <- fetchBlockHeader(blockId)
        currentEpoch  <- clock.epochOf(header.slot)
        previousEpoch <- clock.epochOf(header.parentSlot)
        _             <- Logger[F].info(show"Unapply block $blockId of epoch $currentEpoch")
        _             <- unapplyVoting(state, header, currentEpoch)
        _             <- unapplyNewProposals(state, blockId, currentEpoch)
        _             <- if (currentEpoch != previousEpoch) epochBoundaryCrossed(state, currentEpoch) else ().pure[F]
      } yield state

    private def epochBoundaryCrossed(state: VersionsData[F], currentEpoch: Epoch): F[Unit] =
      for {
        _            <- Logger[F].info(show"Crossing epoch $currentEpoch for unapply block")
        proposalsOpt <- state.epochToProposalIds.get(currentEpoch)
        _            <- proposalsOpt.fold(().pure[F])(clearProposalsVoting(state, _, currentEpoch))
        _            <- state.epochToProposalIds.remove(currentEpoch)

        versionsOpt <- state.epochToVersionIds.get(currentEpoch)
        _           <- versionsOpt.fold(().pure[F])(clearVersions(state, _))
        _           <- state.epochToVersionIds.remove(currentEpoch)
      } yield ()

    private def clearProposalsVoting(
      state:        VersionsData[F],
      proposalIds:  Set[ProposalId],
      currentEpoch: Epoch
    ): F[Unit] =
      Logger[F].info(show"Going to unapply proposals $proposalIds from epoch $currentEpoch") >>
      proposalIds.toList.traverse(id => state.proposalVoting.deleteVoting(currentEpoch, id)).void

    private def clearVersions(
      state:      VersionsData[F],
      versionIds: Set[ProposalId]
    ): F[Unit] = versionIds.toList
      .traverse(version =>
        state.versionCounter.decrementVersion >>
        state.versionIdToProposal.remove(version)
      )
      .void

    private def unapplyVoting(state: VersionsData[F], header: BlockHeader, currentEpoch: Epoch): F[Unit] =
      for {
        votedIdOpt <- header.getProposalVote.pure[F]
        _          <- votedIdOpt.traverse(votedId => state.proposalVoting.removeOneVote((currentEpoch, votedId)))
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

  implicit class proposalVotingOps[F[_]: MonadThrow](storage: Store[F, (Epoch, ProposalId), Long]) {

    def addOneVote(vote: (Epoch, ProposalId)): F[Unit] =
      for {
        currentVote <- storage.getOrRaise(vote)
        _           <- storage.put(vote, currentVote + 1)
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
