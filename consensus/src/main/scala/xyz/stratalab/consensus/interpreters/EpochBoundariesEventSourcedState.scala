package xyz.stratalab.consensus.interpreters

import cats.effect.Async
import cats.implicits._
import co.topl.consensus.models.{BlockId, SlotData}
import xyz.stratalab.algebras.ClockAlgebra.implicits._
import xyz.stratalab.algebras._
import xyz.stratalab.eventtree.{EventSourcedState, ParentChildTree}
import xyz.stratalab.models._
import xyz.stratalab.typeclasses.implicits._

/**
 * An EventSourcedState which operates on an `EpochBoundaries`.
 *
 * Applying a block simply marks that particular block as the epoch boundary for that epoch.
 *
 * Unapplying a block depends on whether or not the block crosses an epoch boundary.  If the block crosses an epoch
 * boundary, the block's epoch is removed from the store.  If the block does not cross an epoch boundary, the block's
 * parent is set to the epoch's boundary.
 */
object EpochBoundariesEventSourcedState {

  // Captures the _last_ block ID of each epoch
  type EpochBoundaries[F[_]] = Store[F, Epoch, BlockId]

  def make[F[_]: Async](
    clock:               ClockAlgebra[F],
    currentBlockId:      F[BlockId],
    parentChildTree:     ParentChildTree[F, BlockId],
    currentEventChanged: BlockId => F[Unit],
    initialState:        F[EpochBoundaries[F]],
    fetchSlotData:       BlockId => F[SlotData]
  ): F[EventSourcedState[F, EpochBoundaries[F], BlockId]] = {
    def applyBlock(state: EpochBoundaries[F], blockId: BlockId) =
      for {
        slotData <- fetchSlotData(blockId)
        epoch    <- clock.epochOf(slotData.slotId.slot)
        _        <- state.put(epoch, blockId)
      } yield state

    def unapplyBlock(state: EpochBoundaries[F], blockId: BlockId) =
      for {
        slotData    <- fetchSlotData(blockId)
        epoch       <- clock.epochOf(slotData.slotId.slot)
        parentEpoch <- clock.epochOf(slotData.parentSlotId.slot)
        _ <-
          if (epoch === parentEpoch) state.put(epoch, slotData.parentSlotId.blockId)
          else state.remove(epoch).as(state)
      } yield state

    EventSourcedState.OfTree.make(
      initialState = initialState,
      initialEventId = currentBlockId,
      applyEvent = applyBlock,
      unapplyEvent = unapplyBlock,
      parentChildTree = parentChildTree,
      currentEventChanged
    )
  }
}
