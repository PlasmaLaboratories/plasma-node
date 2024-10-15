package xyz.stratalab.interpreters

import cats.effect.kernel.Async
import cats.implicits._
import xyz.stratalab.algebras.{Store, StoreReader}
import xyz.stratalab.consensus.models.{BlockId, SlotData}
import xyz.stratalab.eventtree.{EventSourcedState, ParentChildTree}
import xyz.stratalab.typeclasses.implicits._

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
