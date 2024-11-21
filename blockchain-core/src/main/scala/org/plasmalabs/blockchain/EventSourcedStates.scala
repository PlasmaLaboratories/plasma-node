package org.plasmalabs.blockchain

import cats.implicits.*
import cats.{Functor, Parallel}
import org.plasmalabs.blockchain.interpreters.EpochDataEventSourcedState
import org.plasmalabs.consensus.interpreters.*
import org.plasmalabs.consensus.models.BlockId
import org.plasmalabs.eventtree.EventSourcedState
import org.plasmalabs.interpreters.{BlockHeightTree, TxIdToBlockIdTree}
import org.plasmalabs.ledger.interpreters.{BoxState, Mempool, ProposalEventSourceState, RegistrationAccumulator}

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
  crossEpochForkLocal:  EventSourcedState[F, CrossEpochEventSourceState.VotingData[F], BlockId],
  crossEpochForkP2P:    EventSourcedState[F, CrossEpochEventSourceState.VotingData[F], BlockId],
  votingForkLocal:      EventSourcedState[F, VotingEventSourceState.State[F], BlockId],
  votingForkP2P:        EventSourcedState[F, VotingEventSourceState.State[F], BlockId],
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
      votingForkLocal
      // This line is included but intentionally commented out due to the N-2 epoch nature of consensus data
      // proposalLocal,
      // This line is included but intentionally commented out due to those event state is controlled by votingForkLocal
      // crossEpochForkLocal
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
      votingForkP2P
      // This line is included but intentionally commented out due to the N-2 epoch nature of consensus data
      // proposalP2P
      // This line is included but intentionally commented out due to those event state is controlled by votingForkP2P
      // crossEpochForkP2P
    ).parTraverse(_.stateAt(id).void).void
}
