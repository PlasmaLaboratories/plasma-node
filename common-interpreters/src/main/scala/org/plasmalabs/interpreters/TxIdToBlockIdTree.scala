package org.plasmalabs.interpreters

import cats.effect.kernel.Async
import cats.implicits.*
import org.plasmalabs.algebras.Store
import org.plasmalabs.consensus.models.BlockId
import org.plasmalabs.eventtree.{EventSourcedState, ParentChildTree}
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.sdk.models.TransactionId
import org.plasmalabs.typeclasses.implicits.*

object TxIdToBlockIdTree {
  type State[F[_]] = Store[F, TransactionId, BlockId]

  def make[F[_]: Async](
    currentBlockId: F[BlockId],
    blockIdToBody:  BlockId => F[Option[BlockBody]],
    initialState:   State[F],
    blockTree:      ParentChildTree[F, BlockId]
  ): F[EventSourcedState[F, State[F], BlockId]] =
    EventSourcedState.OfTree.make[F, State[F], BlockId](
      initialState = initialState.pure[F],
      initialEventId = currentBlockId,
      applyEvent = (storage: State[F], blockId: BlockId) =>
        blockIdToBody(blockId).flatMap {
          case Some(body) => body.transactionIds.traverse(storage.put(_, blockId)) >> storage.pure[F]
          case None       => storage.pure[F]
        },
      unapplyEvent = (storage: State[F], id: BlockId) =>
        blockIdToBody(id).flatMap {
          case Some(body) => body.transactionIds.traverse(txId => storage.remove(txId)) >> storage.pure[F]
          case None       => storage.pure[F]
        },
      parentChildTree = blockTree,
      _ => Async[F].unit
    )
}
