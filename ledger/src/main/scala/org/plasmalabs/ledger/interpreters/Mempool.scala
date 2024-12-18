package org.plasmalabs.ledger.interpreters

import cats.data.EitherT
import cats.effect.*
import cats.effect.implicits.*
import cats.implicits.*
import fs2.concurrent.Topic
import org.plasmalabs.algebras.{ClockAlgebra, Stats}
import org.plasmalabs.consensus.models.BlockId
import org.plasmalabs.eventtree.{EventSourcedState, ParentChildTree}
import org.plasmalabs.ledger.algebras.{MempoolAlgebra, TransactionRewardCalculatorAlgebra}
import org.plasmalabs.ledger.models.MempoolGraph
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.sdk.models.TransactionId
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.syntax.ioTransactionAsTransactionSyntaxOps
import org.plasmalabs.sdk.validation.algebras.TransactionCostCalculator
import org.plasmalabs.typeclasses.implicits.*

object Mempool {

  type State[F[_]] = Ref[F, MempoolGraph]

  // scalastyle:off method.length
  def make[F[_]: Async: Stats](
    currentBlockId:              F[BlockId],
    fetchBody:                   BlockId => F[BlockBody],
    fetchTransaction:            TransactionId => F[IoTransaction],
    parentChildTree:             ParentChildTree[F, BlockId],
    currentEventChanged:         BlockId => F[Unit],
    clock:                       ClockAlgebra[F],
    onExpiration:                TransactionId => F[Unit],
    defaultExpirationLimit:      Long,
    transactionRewardCalculator: TransactionRewardCalculatorAlgebra,
    txCostCalculator:            TransactionCostCalculator
  ): Resource[F, (MempoolAlgebra[F], EventSourcedState[F, State[F], BlockId])] =
    for {
      graphState <- Ref
        .of(MempoolGraph(Map.empty, Map.empty, Map.empty, transactionRewardCalculator, txCostCalculator))
        .toResource
      expirationsState <- Resource.make(Ref.of(Map.empty[TransactionId, Fiber[F, Throwable, Unit]]))(
        _.get.flatMap(_.values.toList.traverse(_.cancel).void)
      )
      adoptionsTopic <- Resource.make(Topic[F, TransactionId])(_.close.void)
      expireTransaction = (transaction: IoTransaction) =>
        graphState
          .modify(_.removeSubtree(transaction))
          .map(_.map(_.id).toList)
          .flatMap(expired =>
            expirationsState.update(_.removedAll(expired)) *>
            expired.traverse(onExpiration)
          )
      addWithExpiration = (transaction: IoTransaction) =>
        for {
          currentSlot <- clock.globalSlot
          expirationSlot = transaction.datum.event.schedule.max.min(currentSlot + defaultExpirationLimit)
          expirationFiber <-
            clock
              .delayedUntilSlot(expirationSlot)
              .flatMap(_ =>
                for {
                  _ <- expireTransaction(transaction)
                  _ <- Stats[F].incrementCounter(
                    "plasma_node_mempool_transaction_expired",
                    "Counter when a transaction is expired from the mempool",
                    Map()
                  )
                } yield ()
              )
              .void
              .start
          _ <- expirationsState.update(_.updated(transaction.id, expirationFiber))
          _ <- graphState.update(_.add(transaction))
        } yield ()
      removeWithExpiration = (transaction: IoTransaction) =>
        graphState
          .update(_.removeSingle(transaction)) *> expirationsState
          .getAndUpdate(_.removed(transaction.id))
          .flatTap(_.get(transaction.id).traverse(_.cancel))
          .void
      applyBlock = (state: State[F], blockId: BlockId) =>
        for {
          body <- fetchBody(blockId)
          // Note: Do not include reward tranaction
          _ <- body.transactionIds.traverse(fetchTransaction(_).flatMap(removeWithExpiration))
        } yield state
      unapplyBlock = (state: State[F], blockId: BlockId) =>
        for {
          body <- fetchBody(blockId)
          // Note: Do not include reward transaction
          _ <- body.transactionIds.traverse(fetchTransaction(_).flatMap(addWithExpiration))
        } yield state
      eventSourcedState <- EventSourcedState.OfTree
        .make[F, State[F], BlockId](
          graphState.pure[F],
          currentBlockId,
          applyEvent = applyBlock,
          unapplyEvent = unapplyBlock,
          parentChildTree,
          currentEventChanged
        )
        .toResource
      interpreter = new MempoolAlgebra[F] {

        def read(blockId: BlockId): F[MempoolGraph] =
          eventSourcedState
            .useStateAt(blockId)(_.get)

        def add(transactionId: TransactionId): F[Boolean] =
          (fetchTransaction(transactionId).flatMap(addWithExpiration) >> true.pure[F])
            .flatTap(
              Async[F].whenA(_)(
                EitherT(for {
                  adoptionsTopic <- adoptionsTopic.publish1(transactionId)
                  _ <- Stats[F].incrementCounter(
                    "plasma_node_mempool_transaction_published",
                    "Counter when a transaction is published to the mempool topic",
                    Map()
                  )
                } yield (adoptionsTopic))
                  .leftMap(_ => new IllegalStateException("MempoolBroadcaster topic unexpectedly closed"))
                  .rethrowT
              )
            )

        def remove(transactionId: TransactionId): F[Unit] = for {
          _ <- fetchTransaction(transactionId).flatMap(removeWithExpiration)
          _ <- Stats[F].incrementCounter(
            "plasma_node_mempool_transaction_removed",
            "Counter when a transaction is removed from the mempool",
            Map()
          )
        } yield ()

        def contains(blockId: BlockId, transactionId: TransactionId): F[Boolean] =
          eventSourcedState
            .useStateAt(blockId)(_.get)
            .map(_.transactions.contains(transactionId))

        def adoptions: Topic[F, TransactionId] =
          adoptionsTopic
      }
    } yield (interpreter, eventSourcedState)
  // scalastyle:on method.length
}
