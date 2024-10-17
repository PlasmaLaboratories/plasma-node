package org.plasmalabs.interpreters

import cats.effect.kernel.Async
import cats.implicits._
import org.plasmalabs.algebras.{Store, StoreReader}
import org.plasmalabs.consensus.models.{BlockId, SlotData}
import org.plasmalabs.eventtree.{EventSourcedState, ParentChildTree}
import org.plasmalabs.typeclasses.implicits._

object BlockHeightTree {

  type State[F[_]] = Long => F[Option[BlockId]]
  type ESS[F[_]] = EventSourcedState[F, State[F], BlockId]

  def make[F[_]: Async](
    store:               Store[F, Long, BlockId],
    initialEventId:      F[BlockId],
    slotDataStore:       StoreReader[F, BlockId, SlotData],
    blockTree:           ParentChildTree[F, BlockId],
    currentEventChanged: BlockId => F[Unit]
  ): F[ESS[F]] = {
    val heightStore = slotDataStore.mapRead[BlockId, Long](identity, identity, _.height)
    EventSourcedState.OfTree.make[F, State[F], BlockId](
      Async[F].delay(store.get),
      initialEventId = initialEventId,
      (state, id) => heightStore.getOrRaise(id).flatTap(store.put(_, id)).as(state),
      (state, id) => heightStore.getOrRaise(id).flatTap(store.remove).as(state),
      blockTree,
      currentEventChanged
    )
  }

}
