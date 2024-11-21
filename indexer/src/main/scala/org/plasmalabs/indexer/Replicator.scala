package org.plasmalabs.indexer

import cats.data.*
import cats.effect.Async
import cats.effect.implicits.*
import cats.effect.kernel.{Outcome, Resource}
import cats.implicits.*
import org.plasmalabs.algebras.{Stats, SynchronizationTraversalStep, SynchronizationTraversalSteps}
import org.plasmalabs.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import org.plasmalabs.indexer.services.BlockData
import org.plasmalabs.interpreters.NodeRpcOps.*
import org.plasmalabs.typeclasses.implicits.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object Replicator {

  def background[F[_]: Async: Stats](
    indexer: Indexer[F, fs2.Stream[F, *]]
  ): Resource[F, F[Outcome[F, Throwable, Unit]]] =
    stream(indexer).compile.drain.background

  def stream[F[_]: Async: Stats](indexer: Indexer[F, fs2.Stream[F, *]]): fs2.Stream[F, Unit] =
    for {
      given Logger[F] <- fs2.Stream.eval(Slf4jLogger.fromName("Indexer.Replicator"))
      _               <- fs2.Stream.eval(indexer.nodeRpcClient.waitForRpcStartUp)
      nodeLatestHeight <- fs2.Stream.eval(
        OptionT(indexer.nodeBlockFetcher.fetchHeight()).getOrRaise(new IllegalStateException("Unknown node height"))
      )
      graphCurrentHeight <- fs2.Stream.eval(
        OptionT(
          indexer.blockFetcher
            .fetchCanonicalHead()
            .rethrow
        ).fold(0L)(_.height)
      )
      _ <- fs2.Stream.eval(
        Logger[F].info(s"Historical data start=${graphCurrentHeight + 1}, end=${nodeLatestHeight}")
      )
      // Historical + live data streams
      _ <- fs2.Stream
        .force[F, BlockData](
          indexer.nodeBlockFetcher.fetch(startHeight = graphCurrentHeight + 1, endHeight = nodeLatestHeight + 3)
        )
        .evalTap(blockData =>
          Logger[F].info(s"Inserting block ${blockData.header.id.show} height=${blockData.header.height}")
        )
        .evalMap(indexer.blockUpdater.insert)
        .rethrow ++
      fs2.Stream
        .force[F, SynchronizationTraversalStep](
          indexer.nodeRpcClient.synchronizationTraversal()
        )
        .evalMap {
          case SynchronizationTraversalSteps.Applied(blockId) =>
            EitherT(indexer.nodeBlockFetcher.fetch(blockId))
              .semiflatTap(blockData =>
                Logger[F].info(s"Inserting block ${blockData.header.id.show} height=${blockData.header.height}") >>
                Async[F].defer(
                  Stats[F].recordGauge(
                    "plasma_node_indexer_replications",
                    "Indexer replications",
                    Map("block_id" -> stringToJson(blockData.header.id.show)),
                    longToJson(blockData.header.height)
                  )
                )
              )
              .flatMapF(indexer.blockUpdater.insert)
              .value
          case SynchronizationTraversalSteps.Unapplied(blockId) =>
            EitherT(indexer.nodeBlockFetcher.fetch(blockId))
              .semiflatTap(blockData =>
                Logger[F].info(s"Deleting block ${blockData.header.id.show} height=${blockData.header.height}")
              )
              .flatMapF(indexer.blockUpdater.remove)
              .value
        }
        .rethrow

    } yield ()
}
