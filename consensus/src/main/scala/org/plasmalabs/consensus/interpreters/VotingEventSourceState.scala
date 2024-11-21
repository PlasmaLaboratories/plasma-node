package org.plasmalabs.consensus.interpreters

import cats.MonadThrow
import cats.effect.Async
import cats.implicits.*
import org.plasmalabs.algebras.ClockAlgebra
import org.plasmalabs.algebras.ClockAlgebra.implicits.*
import org.plasmalabs.algebras.StoreOps.*
import org.plasmalabs.codecs.bytes.tetra.instances.*
import org.plasmalabs.consensus.interpreters.CrossEpochEventSourceState.VotingData
import org.plasmalabs.consensus.models.{BlockHeader, BlockId}
import org.plasmalabs.eventtree.{EventSourcedState, ParentChildTree}
import org.plasmalabs.models.*
import org.plasmalabs.typeclasses.implicits.*
import org.typelevel.log4cats.Logger

object VotingEventSourceState {

  case class State[F[_]]()

  def make[F[_]: Async: Logger](
    currentBlockId:             F[BlockId],
    parentChildTree:            ParentChildTree[F, BlockId],
    currentEventChanged:        BlockId => F[Unit],
    initialState:               F[State[F]],
    clock:                      ClockAlgebra[F],
    fetchHeader:                BlockId => F[BlockHeader],
    crossEpochEventSourceState: EventSourcedState[F, VotingData[F], BlockId]
  ): F[EventSourcedState[F, State[F], BlockId]] =
    EventSourcedState.OfTree.make(
      initialState = initialState,
      initialEventId = currentBlockId,
      applyEvent = new ApplyBlock(clock, fetchHeader, crossEpochEventSourceState),
      unapplyEvent = new UnapplyBlock(clock, fetchHeader, crossEpochEventSourceState),
      parentChildTree = parentChildTree,
      currentEventChanged
    )

  private class ApplyBlock[F[_]: MonadThrow: Logger](
    clock:                      ClockAlgebra[F],
    fetchBlockHeader:           BlockId => F[BlockHeader],
    crossEpochEventSourceState: EventSourcedState[F, VotingData[F], BlockId]
  ) extends ((State[F], BlockId) => F[State[F]]) {

    def apply(state: State[F], blockId: BlockId): F[State[F]] =
      crossEpochEventSourceState.useStateAt(blockId) { votingData =>
        for {
          header       <- fetchBlockHeader(blockId)
          currentEpoch <- clock.epochOf(header.slot)
          _            <- Logger[F].debug(show"Apply voting block $blockId of epoch $currentEpoch")
          _            <- applyProposalVoting(votingData, header, currentEpoch)
          _            <- applyVersionVoting(votingData, header, currentEpoch)
        } yield State()
      }

    private def applyProposalVoting(state: VotingData[F], header: BlockHeader, currentEpoch: Epoch): F[Unit] =
      for {
        votedIdOpt <- header.getProposalVote.pure[F]
        _          <- votedIdOpt.traverse(votedId => state.proposalVoting.tryToAddOneVote((currentEpoch, votedId)))
      } yield ()

    private def applyVersionVoting(state: VotingData[F], header: BlockHeader, currentEpoch: Epoch): F[Unit] =
      for {
        currentVersion   <- state.versionAlgebra.getVersionForEpoch(currentEpoch)
        versionToVoteOpt <- getVersionToVote(currentVersion, header.getVersionVote).pure[F]
        _ <- versionToVoteOpt.traverse { version =>
          state.epochToVersionIds.addIdToEpoch(currentEpoch, version) >>
          state.versionVoting.createVoteAndAddOneVote((currentEpoch, version))
        }
      } yield ()
  }

  private class UnapplyBlock[F[_]: MonadThrow: Logger](
    clock:                      ClockAlgebra[F],
    fetchBlockHeader:           BlockId => F[BlockHeader],
    crossEpochEventSourceState: EventSourcedState[F, VotingData[F], BlockId]
  ) extends ((State[F], BlockId) => F[State[F]]) {

    def apply(state: State[F], blockId: BlockId): F[State[F]] =
      for {
        header <- fetchBlockHeader(blockId)
        _      <- unapplyVoting(header)
        _      <- crossEpochEventSourceState.stateAt(header.parentHeaderId)
      } yield State()

    private def unapplyVoting(header: BlockHeader): F[Unit] =
      crossEpochEventSourceState.useStateAt(header.id) { votingData =>
        for {
          currentEpoch <- clock.epochOf(header.slot)
          _            <- Logger[F].debug(show"Unapply voting block ${header.id} of epoch $currentEpoch")
          _            <- unapplyProposalVoting(votingData, header, currentEpoch)
          _            <- unapplyVersionVoting(votingData, header, currentEpoch)
        } yield ()
      }

    private def unapplyProposalVoting(state: VotingData[F], header: BlockHeader, currentEpoch: Epoch): F[Unit] =
      for {
        votedIdOpt <- header.getProposalVote.pure[F]
        _          <- votedIdOpt.traverse(votedId => state.proposalVoting.tryToRemoveOneVote((currentEpoch, votedId)))
      } yield ()

    private def unapplyVersionVoting(state: VotingData[F], header: BlockHeader, currentEpoch: Epoch): F[Unit] =
      for {
        currentVersion   <- state.versionAlgebra.getVersionForEpoch(currentEpoch)
        versionToVoteOpt <- getVersionToVote(currentVersion, header.getVersionVote).pure[F]
        _                <- versionToVoteOpt.traverse(vote => state.versionVoting.removeOneVote((currentEpoch, vote)))
      } yield ()
  }

  // we ignore version voting if vote is for current version
  private def getVersionToVote(currentVersion: VersionId, votedVersion: Option[VersionId]): Option[VersionId] =
    votedVersion.flatMap(v => Option.when(v != currentVersion)(v))

}
