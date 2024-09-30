package xyz.stratalab.blockchain

import cats.implicits._
import cats.{Functor, Parallel}
import co.topl.consensus.models.BlockId
import xyz.stratalab.blockchain.interpreters.EpochDataEventSourcedState
import xyz.stratalab.consensus.interpreters.{
  ConsensusDataEventSourcedState,
  EpochBoundariesEventSourcedState,
  VotingEventSourceState
}
import xyz.stratalab.eventtree.EventSourcedState
import xyz.stratalab.interpreters.{BlockHeightTree, TxIdToBlockIdTree}
import xyz.stratalab.ledger.interpreters.{BoxState, Mempool, ProposalEventSourceState, RegistrationAccumulator}

case class EventSourcedStates[F[_]](
  epochDataLocal:       EventSourcedState[F, EpochDataEventSourcedState.State[F], BlockId],
  epochDataP2P:         EventSourcedState[F, EpochDataEventSourcedState.State[F], BlockId],
  blockHeightsLocal:    EventSourcedState[F, BlockHeightTree.State[F], BlockId],
  blockHeightsP2P:      EventSourcedState[F, BlockHeightTree.State[F], BlockId],
  consensusDataLocal:   EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[F], BlockId],
  consensusDataP2P:     EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[F], BlockId],
  epochBoundariesLocal: EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[F], BlockId],
  epochBoundariesP2P:   EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[F], BlockId],
  boxStateLocal:        EventSourcedState[F, BoxState.State[F], BlockId],
  boxStateP2P:          EventSourcedState[F, BoxState.State[F], BlockId],
  mempool:              EventSourcedState[F, Mempool.State[F], BlockId],
  registrationsLocal:   EventSourcedState[F, RegistrationAccumulator.State[F], BlockId],
  registrationsP2P:     EventSourcedState[F, RegistrationAccumulator.State[F], BlockId],
  txIdToBlockIdTree:    EventSourcedState[F, TxIdToBlockIdTree.State[F], BlockId],
  votingLocal:          EventSourcedState[F, VotingEventSourceState.VotingData[F], BlockId],
  votingP2P:            EventSourcedState[F, VotingEventSourceState.VotingData[F], BlockId],
  proposalLocal:        ProposalEventSourceState.ProposalEventSourceStateType[F],
  proposalP2P:          ProposalEventSourceState.ProposalEventSourceStateType[F]
) {

  def updateLocalStatesTo(id: BlockId)(implicit fFunctor: Functor[F], fPar: Parallel[F]): F[Unit] =
    List(
      epochDataLocal,
      blockHeightsLocal,
      // This line is included but intentionally commented out due to the N-2 epoch nature of consensus data
      // consensusDataLocal,
      epochBoundariesLocal,
      boxStateLocal,
      mempool,
      registrationsLocal,
      txIdToBlockIdTree,
      votingLocal
      // This line is included but intentionally commented out due to the N-2 epoch nature of consensus data
      // proposalLocal
    ).parTraverse(_.stateAt(id).void).void

  def updateAllStatesTo(id: BlockId)(implicit fFunctor: Functor[F], fPar: Parallel[F]): F[Unit] =
    updateLocalStatesTo(id) &>
    List(
      epochDataP2P,
      blockHeightsP2P,
      // This line is included but intentionally commented out due to the N-2 epoch nature of consensus data
      // consensusDataP2P,
      epochBoundariesP2P,
      boxStateP2P,
      registrationsP2P,
      votingP2P
      // This line is included but intentionally commented out due to the N-2 epoch nature of consensus data
      // proposalP2P
    ).parTraverse(_.stateAt(id).void).void
}
