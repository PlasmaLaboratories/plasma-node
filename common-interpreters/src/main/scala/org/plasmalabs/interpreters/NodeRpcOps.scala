package org.plasmalabs.interpreters

import cats.data.OptionT
import cats.effect.*
import cats.implicits.*
import cats.{Applicative, MonadThrow}
import fs2.Stream
import org.plasmalabs.algebras.{NodeRpc, SynchronizationTraversalSteps}
import org.plasmalabs.consensus.models.{BlockHeader, BlockId}
import org.plasmalabs.node.models.{FullBlock, FullBlockBody}
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*
import scala.language.implicitConversions

object NodeRpcOps extends NodeRpcOps

trait NodeRpcOps {

  implicit def clientAsNodeRpcApi[F[_]](client: NodeRpc[F, Stream[F, *]]): NodeRpcApi[F] = new NodeRpcApi(client)

}

class NodeRpcApi[F[_]](val client: NodeRpc[F, Stream[F, *]]) extends AnyVal {

  def adoptedHeaders: Stream[F, BlockHeader] =
    Stream
      .force(client.synchronizationTraversal())
      .collect { case SynchronizationTraversalSteps.Applied(id) =>
        id
      }
      .evalMap(client.fetchBlockHeader)
      .map(_.get)

  def waitForRpcStartUp(implicit asyncF: Async[F], loggerF: Logger[F]): F[Unit] =
    for {
      _ <- Logger[F].info("Waiting for RPC to start up")
      genesisHeader <-
        Stream
          .retry(
            client
              .blockIdAtHeight(1)
              .map(_.get)
              .flatMap(client.fetchBlockHeader)
              .map(_.get),
            250.milli,
            identity,
            200
          )
          .compile
          .lastOrError
      _        <- Logger[F].info(s"Node RPC is ready. Awaiting Genesis block timestamp=${genesisHeader.timestamp}")
      duration <- Async[F].realTimeInstant.map(genesisHeader.timestamp - _.toEpochMilli).map(_.milli)
      _        <- Applicative[F].whenA(duration.toMillis > 0)(Async[F].sleep(duration))
      _        <- Logger[F].info("Genesis block timestamp reached.  Node is ready.")
    } yield ()

  def canonicalHeadId(implicit mThrow: MonadThrow[F]): F[BlockId] =
    OptionT(client.blockIdAtDepth(0))
      .getOrRaise(new IllegalArgumentException("Empty blockchain"))

  def canonicalHeadFullBlock(implicit mThrow: MonadThrow[F]): F[FullBlock] =
    OptionT(client.blockIdAtDepth(0))
      .flatMapF(fetchBlock)
      .getOrRaise(new IllegalArgumentException("Empty blockchain"))

  def fetchBlock(id: BlockId)(implicit mThrow: MonadThrow[F]): F[Option[FullBlock]] = (
    for {
      header            <- OptionT(client.fetchBlockHeader(id))
      body              <- OptionT(client.fetchBlockBody(id))
      transactions      <- body.transactionIds.map(client.fetchTransaction).traverse(OptionT(_))
      rewardTransaction <- body.rewardTransactionId.map(client.fetchTransaction).traverse(OptionT(_))
    } yield FullBlock(header, FullBlockBody(transactions, rewardTransaction))
  ).value

  def history(implicit mThrow: MonadThrow[F]): Stream[F, FullBlock] =
    for {
      head <- Stream.eval(canonicalHeadFullBlock)
      headers <- Stream(head) ++ Stream.unfoldEval(head)(child =>
        if (child.header.height == 1)
          none[(FullBlock, FullBlock)].pure[F]
        else
          OptionT(fetchBlock(child.header.parentHeaderId))
            .getOrRaise(new IllegalArgumentException("Missing header"))
            .map(h => (h, h).some)
      )
    } yield headers
}
