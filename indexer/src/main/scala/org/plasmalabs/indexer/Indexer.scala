package org.plasmalabs.indexer

import cats.effect._
import fs2.io.file.Files
import org.plasmalabs.algebras._
import org.plasmalabs.grpc.NodeGrpc
import org.plasmalabs.indexer.algebras._
import org.plasmalabs.indexer.interpreter._
import org.plasmalabs.indexer.orientDb.{OrientDBFactory, OrientThread}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/**
 * Captures the interpreters needed to run Indexer
 */
case class Indexer[F[_], S[_]](
  nodeRpcClient:      NodeRpc[F, S],
  nodeBlockFetcher:   NodeBlockFetcherAlgebra[F, S],
  vertexFetcher:      VertexFetcherAlgebra[F],
  blockFetcher:       BlockFetcherAlgebra[F],
  blockUpdater:       BlockUpdaterAlgebra[F],
  transactionFetcher: TransactionFetcherAlgebra[F],
  valueFetcher:       TokenFetcherAlgebra[F]
)

object Indexer {

  def make[F[_]: Async: Files](
    nodeRpcHost:      String,
    nodeRpcPort:      Int,
    nodeRpcTls:       Boolean,
    dataDir:          String,
    dbPassword:       String,
    fetchConcurrency: Int = 64
  ): Resource[F, Indexer[F, fs2.Stream[F, *]]] =
    for {
      implicit0(logger: Logger[F]) <- Resource.pure(Slf4jLogger.getLoggerFromName[F]("Indexer"))
      // A dedicated single thread executor in which all OrientDB calls are expected to run
      implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
      orientdb                                 <- OrientDBFactory.make[F](dataDir, dbPassword)

      dbTx <- Resource
        .eval(Async[F].delay(orientdb.getTx))
        .evalTap(db => orientThread.delay(db.makeActive()))
      dbNoTx <- Resource
        .eval(Async[F].delay(orientdb.getNoTx))
        .evalTap(db => orientThread.delay(db.makeActive()))

      rpcInterpreter   <- NodeGrpc.Client.make[F](nodeRpcHost, nodeRpcPort, tls = nodeRpcTls)
      nodeBlockFetcher <- NodeBlockFetcher.make(rpcInterpreter, fetchConcurrency)

      vertexFetcher      <- GraphVertexFetcher.make[F](dbNoTx)
      blockFetcher       <- GraphBlockFetcher.make(vertexFetcher)
      graphBlockUpdater  <- GraphBlockUpdater.make[F](dbTx, blockFetcher, nodeBlockFetcher)
      transactionFetcher <- GraphTransactionFetcher.make(vertexFetcher)
      valueFetcher       <- GraphTokenFetcher.make(vertexFetcher)
    } yield Indexer(
      rpcInterpreter,
      nodeBlockFetcher,
      vertexFetcher,
      blockFetcher,
      graphBlockUpdater,
      transactionFetcher,
      valueFetcher
    )
}
