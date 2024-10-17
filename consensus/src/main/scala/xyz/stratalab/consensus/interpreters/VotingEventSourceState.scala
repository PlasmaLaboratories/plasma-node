package xyz.stratalab.consensus.interpreters

import cats._
import cats.effect.Async
import cats.implicits._
import org.typelevel.log4cats.Logger
import xyz.stratalab.algebras.ClockAlgebra.implicits._
import xyz.stratalab.algebras.StoreOps._
import xyz.stratalab.algebras._
import xyz.stratalab.consensus.algebras.VersionInfoAlgebra
import xyz.stratalab.consensus.interpreters.EpochBoundariesEventSourcedState.EpochBoundaries
import xyz.stratalab.consensus.models.{BlockHeader, BlockId}
import xyz.stratalab.eventtree.{EventSourcedState, ParentChildTree}
import xyz.stratalab.ledger.interpreters.ProposalEventSourceState
import xyz.stratalab.ledger.interpreters.ProposalEventSourceState.{ProposalData, ProposalEventSourceStateType}
import xyz.stratalab.models._
import xyz.stratalab.proto.node.EpochData
import xyz.stratalab.sdk.models.box.Value.ConfigProposal
import xyz.stratalab.typeclasses.implicits._

object VotingEventSourceState {

  case class VotingData[F[_]](
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
    versionIdToProposal: Store[F, VersionId, ConfigProposal],
    versionCounter:      Store[F, Unit, VersionId],
    versionVoting:       Store[F, (Epoch, VersionId), Long],
    // Describes from which era which version starts
    versionAlgebra: VersionInfoAlgebra[F]
  )

  private def getTargetEpoch(epoch: Epoch, config: ProposalConfig): Epoch =
    epoch + config.versionSwitchWindow

  private type UseProposalStateType[F[_]] = (BlockId, Epoch) => (ProposalData[F] => F[Unit]) => F[Unit]

  def make[F[_]: Async: Logger](
    currentBlockId:                 F[BlockId],
    parentChildTree:                ParentChildTree[F, BlockId],
    currentEventChanged:            BlockId => F[Unit],
    initialState:                   F[VotingData[F]],
    clock:                          ClockAlgebra[F],
    fetchHeader:                    BlockId => F[BlockHeader],
    epochBoundaryEventSourcedState: EventSourcedState[F, EpochBoundaries[F], BlockId],
    epochData:                      EventSourcedState[F, Store[F, Epoch, EpochData], BlockId],
    proposalState:                  ProposalEventSourceStateType[F],
    genesisBlockId:                 BlockId,
    config:                         ProposalConfig
  ): F[EventSourcedState[F, VotingData[F], BlockId]] = {
    val useProposalStateAt: UseProposalStateType[F] =
      useStateAtTargetBoundary[F, Unit](epochBoundaryEventSourcedState, proposalState, genesisBlockId)

    EventSourcedState.OfTree.make(
      initialState = initialState,
      initialEventId = currentBlockId,
      applyEvent = new ApplyBlock(clock, fetchHeader, useProposalStateAt, epochData, config),
      unapplyEvent = new UnapplyBlock(clock, fetchHeader, useProposalStateAt, config),
      parentChildTree = parentChildTree,
      currentEventChanged
    )
  }

  private class ApplyBlock[F[_]: MonadThrow: Logger](
    clock:              ClockAlgebra[F],
    fetchBlockHeader:   BlockId => F[BlockHeader],
    useProposalStateAt: UseProposalStateType[F],
    epochDataState:     EventSourcedState[F, Store[F, Epoch, EpochData], BlockId],
    config:             ProposalConfig
  ) extends ((VotingData[F], BlockId) => F[VotingData[F]]) {

    import ApplyBlock._

    def apply(state: VotingData[F], blockId: BlockId): F[VotingData[F]] =
      for {
        header        <- fetchBlockHeader(blockId)
        currentEpoch  <- clock.epochOf(header.slot)
        previousEpoch <- clock.epochOf(header.parentSlot)
        _             <- Logger[F].debug(show"Apply block $blockId of epoch $currentEpoch")

        _ <-
          if (currentEpoch != previousEpoch) epochBoundaryCrossed(state, blockId, previousEpoch, currentEpoch)
          else ().pure[F]
        _ <- applyProposalVoting(state, header, currentEpoch) // we could create and vote for proposal in the same block
        _ <- applyVersionVoting(state, header, currentEpoch)
      } yield state

    private def applyProposalVoting(state: VotingData[F], header: BlockHeader, currentEpoch: Epoch): F[Unit] =
      for {
        votedIdOpt <- header.getProposalVote.pure[F]
        _          <- votedIdOpt.traverse(votedId => state.proposalVoting.addOneVote((currentEpoch, votedId)))
      } yield ()

    private def applyVersionVoting(state: VotingData[F], header: BlockHeader, currentEpoch: Epoch): F[Unit] =
      for {
        votedVersionOpt <- header.getVersionVote.pure[F]
        _ <- votedVersionOpt.traverse { version =>
          state.epochToVersionIds.addIdToEpoch(currentEpoch, version) >>
          state.versionVoting.createVoteAndAddOneVote((currentEpoch, version))
        }
      } yield ()

    private def epochBoundaryCrossed(
      state:         VotingData[F],
      blockId:       BlockId,
      previousEpoch: Epoch,
      currentEpoch:  Epoch
    ): F[Unit] =
      useProposalStateAt(blockId, currentEpoch) { proposalData =>
        epochDataState.useStateAt(blockId) { epochData =>
          for {
            _                  <- Logger[F].info(show"Crossing epoch $previousEpoch in voting")
            prevProposals      <- state.epochToProposalIds.get(previousEpoch)
            prevVersions       <- state.epochToVersionIds.get(previousEpoch)
            newActiveProposals <- proposalData.epochToCreatedProposalIds.get(currentEpoch)
            _ <- Logger[F].info(show"Epoch $previousEpoch: active proposals $prevProposals; versions $prevVersions")
            _ <- Logger[F].info(show"Epoch $currentEpoch: new active proposals $newActiveProposals")
            _ <- prevProposals match {
              case Some(proposalIds) =>
                processPreviousEpochProposals(state, proposalData, previousEpoch, currentEpoch, proposalIds, epochData)
              case None => ().pure[F]
            }
            _ <- prevVersions match {
              case Some(versionIds) =>
                processPreviousEpochVersions(state, previousEpoch, currentEpoch, versionIds, epochData)
              case None => ().pure[F]
            }

            _ <- newActiveProposals match {
              case Some(proposalIds) =>
                proposalIds.toList.traverse(processNewActiveProposalIds(state, currentEpoch, _)).void
              case None => ().pure[F]
            }
          } yield ()
        }
      }

    private def processPreviousEpochProposals(
      state:         VotingData[F],
      proposalData:  ProposalData[F],
      previousEpoch: Epoch,
      currentEpoch:  Epoch,
      proposalIds:   Set[ProposalId],
      epochData:     Store[F, Epoch, EpochData]
    ): F[Unit] =
      for {
        votingMaxWindow <- getVotingRangeForMaxWindow(previousEpoch).pure[F]
        blocksInEpochs  <- getEpochsBlockCounts(epochData, votingMaxWindow)
        // We MUST sort proposals for to have the same proposal processing orders
        proposalResults <- proposalIds.toList.sorted.traverse(getProposalResult(_, state, blocksInEpochs))
        proposalActions <- proposalResults.map { case (id, votes) => id -> getResultForProposal(votes) }.pure[F]
        _ <- proposalActions.traverse { case (id, act) => doProposalAction(proposalData, id, act, currentEpoch, state) }
      } yield ()

    private def getVotingRangeForMaxWindow(epoch: Epoch) =
      Range.Long.inclusive(Math.max(0, epoch - config.proposalVotingMaxWindow), epoch, 1)

    private def getEpochsBlockCounts(
      epochData:   Store[F, Epoch, EpochData],
      votingRange: Seq[Epoch]
    ): F[Seq[(Epoch, Long)]] =
      votingRange
        .traverse(e => epochData.getOrRaise(e).map(d => e -> (1 + d.endHeight - d.startHeight)))

    // return voting result for proposal for "block count" epochs
    // Some(true) -- enough voting for proposal
    // Some(false) -- not enough voting for proposal
    // None -- No voting was active for epoch
    private def getProposalResult(
      proposal:    ProposalId,
      state:       VotingData[F],
      blocksCount: Seq[(Epoch, Long)]
    ): F[(ProposalId, Seq[Option[Boolean]])] =
      blocksCount
        .traverse { case (e, blockCount) =>
          state.proposalVoting
            .get((e, proposal))
            .map(_.map(_ / blockCount.toDouble))
            .flatTap(p => Logger[F].debug(show"Voting result for proposal $proposal epoch $e: $p"))
            .map(_.map(_ >= config.configProposalPercentage))
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
      proposalData: ProposalData[F],
      proposalId:   ProposalId,
      result:       ProposalAction,
      currentEpoch: Epoch,
      state:        VotingData[F]
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
          proposalData.idToProposal
            .getOrRaise(proposalId)
            .flatMap(p => proposalToVersion(state, currentEpoch, proposalId, p))
      }

    private def proposalToVersion(
      state:        VotingData[F],
      currentEpoch: Epoch,
      proposalId:   ProposalId,
      proposal:     ConfigProposal
    ): F[Unit] =
      for {
        versionId <- state.versionCounter.getFreeVersion()
        _         <- state.epochToCreatedVersionIds.addIdToEpoch(currentEpoch, versionId)
        _         <- state.versionIdToProposal.put(versionId, proposal)
        _         <- Logger[F].info(show"Created new version $versionId from proposal $proposalId")
      } yield ()

    private def processPreviousEpochVersions(
      state:         VotingData[F],
      previousEpoch: Epoch,
      currentEpoch:  Epoch,
      versionIds:    Set[VersionId],
      epochData:     Store[F, Epoch, EpochData]
    ): F[Unit] =
      for {
        votingWindow   <- getVersionVotingEpochs(previousEpoch).pure[F]
        blocksInEpochs <- getEpochsBlockCounts(epochData, votingWindow)
        versionResults <- versionIds.toList.sorted.traverse(getVersionResult(_, state, blocksInEpochs))
        versionActions <- versionResults.map { case (id, votes) => id -> getResultForVersion(votes) }.pure[F]
        newVersions    <- versionActions.collect { case (version, VersionAction.toActive) => version }.pure[F]
        _ <- newVersions.ensuring(_.sizeIs < 2, show"More than one version became active: $newVersions").pure[F]
        targetEpoch <- getTargetEpoch(currentEpoch, config).pure[F]
        _           <- newVersions.traverse(doVersionAction(state, targetEpoch, _))
      } yield ()

    private def getVersionVotingEpochs(epoch: Epoch) =
      Range.Long.inclusive(Math.max(0, epoch - config.versionVotingWindow), epoch, 1)

    // return voting result for version for "block count" epochs
    // true -- enough voting for version
    // false -- not enough voting for version
    private def getVersionResult(
      version:     VersionId,
      state:       VotingData[F],
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

    private def doVersionAction(state: VotingData[F], targetEpoch: Epoch, version: VersionId): F[Unit] =
      for {
        _ <- Logger[F].info(show"Version $version passed voting. New version will be active from $targetEpoch epoch")
        _ <- state.versionAlgebra.addVersionStartEpoch(targetEpoch, version)
      } yield ()

    private def processNewActiveProposalIds(
      state:        VotingData[F],
      currentEpoch: Epoch,
      proposalId:   ProposalId
    ): F[Unit] =
      for {
        _ <- state.epochToProposalIds.addIdToEpoch(currentEpoch, proposalId)
        _ <- state.proposalVoting.put((currentEpoch, proposalId), 0)
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
    clock:              ClockAlgebra[F],
    fetchBlockHeader:   BlockId => F[BlockHeader],
    useProposalStateAt: UseProposalStateType[F],
    config:             ProposalConfig
  ) extends ((VotingData[F], BlockId) => F[VotingData[F]]) {

    def apply(state: VotingData[F], blockId: BlockId): F[VotingData[F]] =
      for {
        header        <- fetchBlockHeader(blockId)
        currentEpoch  <- clock.epochOf(header.slot)
        previousEpoch <- clock.epochOf(header.parentSlot)
        _             <- Logger[F].debug(show"Unapply block $blockId of epoch $currentEpoch")
        _             <- unapplyProposalVoting(state, header, currentEpoch)
        _             <- unapplyVersionVoting(state, header, currentEpoch)
        _ <- if (currentEpoch != previousEpoch) epochBoundaryCrossed(blockId, state, currentEpoch) else ().pure[F]
      } yield state

    private def epochBoundaryCrossed(blockId: BlockId, state: VotingData[F], currentEpoch: Epoch): F[Unit] =
      // we shall call it because proposal state are not updated by applying/unapplying new blocks
      useProposalStateAt(blockId, currentEpoch) { _ =>
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

          _ <- state.versionAlgebra.removeVersionStartEpoch(getTargetEpoch(currentEpoch, config))

        } yield ()
      }

    private def clearProposalsVoting(
      state:        VotingData[F],
      proposalIds:  Set[ProposalId],
      currentEpoch: Epoch
    ): F[Unit] =
      Logger[F].info(show"Going to unapply proposals $proposalIds from epoch $currentEpoch") >>
      proposalIds.toList.traverse(id => state.proposalVoting.deleteVoting(currentEpoch, id)).void

    private def clearVotedVersions(
      state:        VotingData[F],
      currentEpoch: Epoch,
      versionIds:   Set[ProposalId]
    ): F[Unit] = versionIds.toList.traverse { v =>
      state.epochToVersionIds.remove(currentEpoch) >>
      state.versionVoting.remove((currentEpoch, v))
    }.void

    private def clearCreatedVersions(
      state:      VotingData[F],
      versionIds: Set[ProposalId]
    ): F[Unit] = versionIds.toList
      .traverse(version =>
        state.versionCounter.decrementVersion >>
        state.versionIdToProposal.remove(version)
      )
      .void

    private def unapplyProposalVoting(state: VotingData[F], header: BlockHeader, currentEpoch: Epoch): F[Unit] =
      for {
        votedIdOpt <- header.getProposalVote.pure[F]
        _          <- votedIdOpt.traverse(votedId => state.proposalVoting.removeOneVote((currentEpoch, votedId)))
      } yield ()

    private def unapplyVersionVoting(state: VotingData[F], header: BlockHeader, currentEpoch: Epoch): F[Unit] =
      for {
        votedVersionOpt <- header.getVersionVote.pure[F]
        _               <- votedVersionOpt.traverse(vote => state.versionVoting.removeOneVote((currentEpoch, vote)))
      } yield ()

  }

  /**
   * Determines the N-2 epoch from the given block, then determines the final block ID of the N-2 epoch.  That
   * N-2 block is used in determining the `ProposalsState` to retrieve.  Once retrieved, it is applied to the
   * given `f` function
   */
  def useStateAtTargetBoundary[F[_]: Async, Res](
    epochBoundaryEventSourcedState: EventSourcedState[F, EpochBoundaries[F], BlockId],
    proposalState:                  ProposalEventSourceStateType[F],
    genesisBlockId:                 BlockId
  )(currentBlockId: BlockId, currentBlockEpoch: Epoch)(f: ProposalEventSourceState.ProposalData[F] => F[Res]): F[Res] =
    for {
      // Note: Blocks created within the first two epochs should use the state from the genesis block
      boundaryBlockId <-
        if (currentBlockEpoch > 1)
          epochBoundaryEventSourcedState.useStateAt(currentBlockId)(_.getOrRaise(currentBlockEpoch - 2))
        else
          genesisBlockId.pure[F]
      res <- proposalState.useStateAt(boundaryBlockId)(f)
    } yield res
}
