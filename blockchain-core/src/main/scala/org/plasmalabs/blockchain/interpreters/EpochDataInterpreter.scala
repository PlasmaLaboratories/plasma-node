package org.plasmalabs.blockchain.interpreters

import cats.effect.implicits.*
import cats.effect.kernel.Sync
import cats.effect.{Async, Resource}
import cats.implicits.*
import cats.{Applicative, MonadThrow}
import org.plasmalabs.algebras.ClockAlgebra.implicits.*
import org.plasmalabs.algebras.{ClockAlgebra, Stats, Store}
import org.plasmalabs.blockchain.algebras.EpochDataAlgebra
import org.plasmalabs.codecs.bytes.tetra.TetraScodecCodecs
import org.plasmalabs.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import org.plasmalabs.consensus.interpreters.{ConsensusDataEventSourcedState, EpochBoundariesEventSourcedState}
import org.plasmalabs.consensus.models.{BlockHeader, BlockId}
import org.plasmalabs.eventtree.{EventSourcedState, ParentChildTree}
import org.plasmalabs.ledger.algebras.TransactionRewardCalculatorAlgebra
import org.plasmalabs.models.*
import org.plasmalabs.models.utility.*
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.numerics.implicits.*
import org.plasmalabs.proto.node.EpochData
import org.plasmalabs.sdk.common.ContainsImmutable
import org.plasmalabs.sdk.models.TransactionId
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.syntax.*
import org.plasmalabs.typeclasses.implicits.*

/**
 * Invokes an EpochDataEventSourcedState implementation along the chain's canonical head to produce EpochData
 */
object EpochDataInterpreter {

  /**
   * Implements the EpochDataAlgebra
   * @param fetchCanonicalHead a function to retrieve the current canonical head ID.  This will be invoked multiple times over the runtime of the program.
   * @param epochDataEventSourcedState an implementation of a backing EventSourcedState
   */
  def make[F[_]: MonadThrow](
    fetchCanonicalHead:         F[BlockId],
    epochDataEventSourcedState: EventSourcedState[F, EpochDataEventSourcedState.State[F], BlockId]
  ): Resource[F, EpochDataAlgebra[F]] =
    Resource.pure((epoch: Epoch) => fetchCanonicalHead >>= (epochDataEventSourcedState.useStateAt(_)(_.get(epoch))))
}

/**
 * An Event-Sourced State implementation which tracks Epoch Data accumulations over epochs.
 */
object EpochDataEventSourcedState {

  type State[F[_]] = Store[F, Epoch, EpochData]

  /**
   * Implements an EventSourcedState which tracks/accumulates data as blocks are applied
   * @param currentBlockId The initial ID when launching the interpreter
   * @param genesisBlockId The chain's genesis block ID
   * @param parentChildTree A block ID parent-child tree
   * @param currentEventChanged A callback function that is invoked whenever a block is applied or unapplied
   * @param initialState The initial state of the event-sourced state at `currentBlockId`
   * @param clock a clock
   * @param fetchBlockHeader lookup a block header by ID
   * @param fetchBlockBody lookup a block body by ID
   * @param fetchTransaction lookup a transaction by ID
   * @param transactionRewardCalculator calculate a transaction's rewards
   * @param epochBoundaryEventSourcedState an event-sourced state which tracks the last block of each epoch
   * @param consensusDataEventSourcedState an event-sourced state which tracks staking information
   */
  def make[F[_]: Async: Stats](
    currentBlockId:              F[BlockId],
    genesisBlockId:              BlockId,
    parentChildTree:             ParentChildTree[F, BlockId],
    currentEventChanged:         BlockId => F[Unit],
    initialState:                F[State[F]],
    clock:                       ClockAlgebra[F],
    fetchBlockHeader:            BlockId => F[BlockHeader],
    fetchBlockBody:              BlockId => F[BlockBody],
    fetchTransaction:            TransactionId => F[IoTransaction],
    transactionRewardCalculator: TransactionRewardCalculatorAlgebra,
    epochBoundaryEventSourcedState: EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[
      F
    ], BlockId],
    consensusDataEventSourcedState: EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[
      F
    ], BlockId]
  ): Resource[F, EventSourcedState[F, State[F], BlockId]] =
    EventSourcedState.OfTree
      .make(
        initialState = initialState,
        initialEventId = currentBlockId,
        applyEvent = new ApplyBlock(
          genesisBlockId,
          clock,
          fetchBlockHeader,
          fetchBlockBody,
          fetchTransaction,
          transactionRewardCalculator,
          epochBoundaryEventSourcedState,
          consensusDataEventSourcedState
        ),
        unapplyEvent =
          new UnapplyBlock(clock, fetchBlockHeader, fetchBlockBody, fetchTransaction, transactionRewardCalculator),
        parentChildTree = parentChildTree,
        currentEventChanged
      )
      .toResource

  private class ApplyBlock[F[_]: Sync: Stats](
    genesisBlockId:              BlockId,
    clock:                       ClockAlgebra[F],
    fetchBlockHeader:            BlockId => F[BlockHeader],
    fetchBlockBody:              BlockId => F[BlockBody],
    fetchTransaction:            TransactionId => F[IoTransaction],
    transactionRewardCalculator: TransactionRewardCalculatorAlgebra,
    epochBoundaryEventSourcedState: EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[
      F
    ], BlockId],
    consensusDataEventSourcedState: EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[
      F
    ], BlockId]
  ) extends ((State[F], BlockId) => F[State[F]]) {

    def apply(state: State[F], blockId: BlockId): F[State[F]] =
      for {
        header      <- fetchBlockHeader(blockId)
        epoch       <- clock.epochOf(header.slot)
        parentEpoch <- clock.epochOf(header.parentSlot)
        _ <-
          if (epoch != parentEpoch) epochBoundaryCrossed(state)(header, epoch)
          else epochBoundaryNotCrossed(state)(header, epoch)
      } yield state

    /**
     * Applies a block which starts a new epoch.  Applying this block will also complete the previous epoch.
     * @param state The current state
     * @param header The new header
     * @param epoch The new epoch
     */
    private def epochBoundaryCrossed(state: State[F])(header: BlockHeader, epoch: Epoch) =
      for {
        // Update the previous epoch entry (unless this is the genesis/-1th epoch)
        _ <- Applicative[F].whenA(epoch >= 0)(
          state
            .getOrRaise(epoch - 1)
            .map(_.copy(isComplete = true))
            .flatMap(state.put(epoch - 1, _))
        )
        // Active/Inactive Stake calculation is delayed by 2 epochs
        stakesBoundaryBlock <-
          if (epoch >= 1)
            epochBoundaryEventSourcedState.useStateAt(header.id)(_.getOrRaise(epoch - 2))
          else
            genesisBlockId.pure[F]
        (activeStake, inactiveStake) <-
          consensusDataEventSourcedState.useStateAt(stakesBoundaryBlock)(s =>
            (s.totalActiveStake.getOrRaise(()), s.totalInactiveStake.getOrRaise(())).tupled
          )
        newEpochBoundary <- clock.epochRange(epoch)
        startTimestamp   <- clock.slotToTimestamps(newEpochBoundary.start).map(_.start)
        endTimestamp     <- clock.slotToTimestamps(newEpochBoundary.end).map(_.end)
        newEpochDataBase = EpochData(
          epoch = epoch,
          eon = 1, // Hardcoded for now
          era = 0, // Hardcoded for now
          isComplete = false,
          startHeight = header.height,
          endHeight = header.height,
          startSlot = newEpochBoundary.start,
          endSlot = newEpochBoundary.end,
          startTimestamp = startTimestamp,
          endTimestamp = endTimestamp,
          transactionCount = 0,
          totalTransactionReward = 0,
          activeStake = activeStake,
          inactiveStake = inactiveStake,
          dataBytes = TetraScodecCodecs.consensusBlockHeaderCodec.encode(header).require.length
        )
        newEpochData <- applyTransactions(newEpochDataBase)(header)
        _            <- state.put(epoch, newEpochData)
        _ <- Stats[F].recordGauge(
          "plasma_node_epoch_timestamp",
          "Timestamp of the epoch.",
          Map("epoch" -> stringToJson(epoch.toString)),
          longToJson(newEpochData.startTimestamp)
        )
        _ <- Stats[F].recordGauge(
          "plasma_node_epoch",
          "Current value of the Epoch.",
          Map(),
          longToJson(newEpochData.epoch)
        )
        _ <- Stats[F].recordGauge(
          "plasma_node_epoch_eon",
          "Current value of the Eon.",
          Map(),
          longToJson(newEpochData.eon)
        )
        _ <- Stats[F].recordGauge(
          "plasma_node_epoch_era",
          "Current value of the Era.",
          Map(),
          longToJson(newEpochData.era)
        )
        _ <- Stats[F].recordGauge(
          "plasma_node_epoch_transaction_count",
          "Current value of the Era.",
          Map("epoch" -> stringToJson(epoch.toString)),
          longToJson(newEpochData.transactionCount)
        )
      } yield state

    /**
     * Applies a block in the middle of an epoch.
     *
     * @param state  The current state
     * @param header The new header
     * @param epoch  The current epoch
     */
    private def epochBoundaryNotCrossed(state: State[F])(header: BlockHeader, epoch: Epoch) =
      for {
        previousEpochData <- state.getOrRaise(epoch)
        modifiedEpochData = previousEpochData.copy(
          endHeight = header.height,
          dataBytes =
            previousEpochData.dataBytes + TetraScodecCodecs.consensusBlockHeaderCodec.encode(header).require.length
        )
        newEpochData <- applyTransactions(modifiedEpochData)(header)
        _            <- state.put(epoch, newEpochData)
      } yield state

    /**
     * Applies the transaction count and reward accumulation to the given EpochData
     * @param epochData the current EpochData
     * @param header the new header
     * @return a new EpochData
     */
    private def applyTransactions(epochData: EpochData)(header: BlockHeader): F[EpochData] =
      fetchBlockBody(header.id)
        .flatMap(body =>
          if (body.allTransactionIds.isEmpty) epochData.pure[F]
          else
            body.allTransactionIds.foldLeftM(epochData) { case (epochData, id) =>
              fetchTransaction(id)
                .flatMap(transaction =>
                  (
                    // TODO: Read reward transaction from body
                    transactionRewardCalculator.rewardsOf(transaction).pure[F].map(_.lvl),
                    Sync[F].delay(
                      ContainsImmutable.instances.ioTransactionImmutable.immutableBytes(transaction).value.size()
                    )
                  )
                    .mapN((reward, size) =>
                      epochData.copy(
                        transactionCount = epochData.transactionCount + 1,
                        totalTransactionReward = epochData.totalTransactionReward + reward,
                        dataBytes = epochData.dataBytes + size
                      )
                    )
                )
            }
        )

  }

  private class UnapplyBlock[F[_]: Sync](
    clock:                       ClockAlgebra[F],
    fetchBlockHeader:            BlockId => F[BlockHeader],
    fetchBlockBody:              BlockId => F[BlockBody],
    fetchTransaction:            TransactionId => F[IoTransaction],
    transactionRewardCalculator: TransactionRewardCalculatorAlgebra
  ) extends ((State[F], BlockId) => F[State[F]]) {

    def apply(state: State[F], blockId: BlockId): F[State[F]] =
      for {
        header      <- fetchBlockHeader(blockId)
        epoch       <- clock.epochOf(header.slot)
        parentEpoch <- clock.epochOf(header.parentSlot)
        _ <-
          if (epoch != parentEpoch) epochBoundaryCrossed(state)(epoch)
          else epochBoundaryNotCrossed(state)(header, epoch)
      } yield state

    /**
     * Unapplies a block which traversed an epoch boundary.  The block's epoch's Data is removed from state, and the
     * previous epoch is marked as non-complete.
     * @param state the current state
     * @param epoch the unapplied block's epoch
     */
    private def epochBoundaryCrossed(state: State[F])(epoch: Epoch) =
      for {
        previousEpochData <- state.getOrRaise(epoch - 1)
        updatedPreviousEpochData = previousEpochData.copy(isComplete = epoch < 1)
        _ <- state.put(epoch - 1, updatedPreviousEpochData)
        _ <- state.remove(epoch)
      } yield state

    /**
     * Unapplies a block in the middle of an epoch
     * @param state the current state
     * @param header the header to unapply
     * @param epoch the header's epoch
     */
    private def epochBoundaryNotCrossed(state: State[F])(header: BlockHeader, epoch: Epoch) =
      for {
        previousEpochData <- state.getOrRaise(epoch)
        modifiedEpochData = previousEpochData.copy(
          endHeight = header.height - 1,
          dataBytes =
            previousEpochData.dataBytes - TetraScodecCodecs.consensusBlockHeaderCodec.encode(header).require.length
        )
        newEpochData <- unapplyTransactions(modifiedEpochData)(header)
        _            <- state.put(epoch, newEpochData)
      } yield state

    /**
     * Unapplies the transaction count and rewards from the given block
     * @param epochData the current epoch data
     * @param header the header to unapply
     * @return a new EpochData
     */
    private def unapplyTransactions(epochData: EpochData)(header: BlockHeader): F[EpochData] =
      fetchBlockBody(header.id)
        .flatMap(body =>
          if (body.allTransactionIds.isEmpty) epochData.pure[F]
          else
            body.allTransactionIds.reverse.foldLeftM(epochData) { case (epochData, id) =>
              fetchTransaction(id)
                .flatMap(transaction =>
                  (
                    // TODO: Read reward transaction from body
                    transactionRewardCalculator.rewardsOf(transaction).pure[F].map(_.lvl),
                    Sync[F].delay(
                      ContainsImmutable.instances.ioTransactionImmutable.immutableBytes(transaction).value.size()
                    )
                  )
                    .mapN((reward, size) =>
                      epochData.copy(
                        transactionCount = epochData.transactionCount - 1,
                        totalTransactionReward = epochData.totalTransactionReward - reward,
                        dataBytes = epochData.dataBytes - size
                      )
                    )
                )
            }
        )

  }
}
