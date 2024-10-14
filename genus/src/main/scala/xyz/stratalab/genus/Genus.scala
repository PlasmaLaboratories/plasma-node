package xyz.stratalab.genus

import cats.effect._
import fs2.io.file.Files
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import xyz.stratalab.algebras._
import xyz.stratalab.genus.algebras._
import xyz.stratalab.genus.interpreter._
import xyz.stratalab.genus.orientDb.{OrientDBFactory, OrientThread}
import xyz.stratalab.grpc.NodeGrpc

/**
 * Captures the interpreters needed to run Genus
 */
case class Genus[F[_], S[_]](
  nodeRpcClient:      NodeRpc[F, S],
  nodeBlockFetcher:   NodeBlockFetcherAlgebra[F, S],
  vertexFetcher:      VertexFetcherAlgebra[F],
  blockFetcher:       BlockFetcherAlgebra[F],
  blockUpdater:       BlockUpdaterAlgebra[F],
  transactionFetcher: TransactionFetcherAlgebra[F],
  valueFetcher:       TokenFetcherAlgebra[F]
)

object Genus {

  def make[F[_]: Async: Files](
    nodeRpcHost:      String,
    nodeRpcPort:      Int,
    nodeRpcTls:       Boolean,
    dataDir:          String,
    dbPassword:       String,
    fetchConcurrency: Int = 64
  ): Resource[F, Genus[F, fs2.Stream[F, *]]] =
    for {
      given Logger[F] <- Resource.pure(Slf4jLogger.getLoggerFromName[F]("Genus"))
      // A dedicated single thread executor in which all OrientDB calls are expected to run
      given OrientThread[F] <- OrientThread.create[F]
      orientdb <- OrientDBFactory.make[F](dataDir, dbPassword)

      dbTx <- Resource
        .eval(Async[F].delay(orientdb.getTx))
        .evalTap(db => OrientThread[F].delay(db.makeActive()))
      dbNoTx <- Resource
        .eval(Async[F].delay(orientdb.getNoTx))
        .evalTap(db => OrientThread[F].delay(db.makeActive()))

      rpcInterpreter   <- NodeGrpc.Client.make[F](nodeRpcHost, nodeRpcPort, tls = nodeRpcTls)
      nodeBlockFetcher <- NodeBlockFetcher.make(rpcInterpreter, fetchConcurrency)

      vertexFetcher      <- GraphVertexFetcher.make[F](dbNoTx)
      blockFetcher       <- GraphBlockFetcher.make(vertexFetcher)
      graphBlockUpdater  <- GraphBlockUpdater.make[F](dbTx, blockFetcher, nodeBlockFetcher)
      transactionFetcher <- GraphTransactionFetcher.make(vertexFetcher)
      valueFetcher       <- GraphTokenFetcher.make(vertexFetcher)
    } yield Genus(
      rpcInterpreter,
      nodeBlockFetcher,
      vertexFetcher,
      blockFetcher,
      graphBlockUpdater,
      transactionFetcher,
      valueFetcher
    )
}