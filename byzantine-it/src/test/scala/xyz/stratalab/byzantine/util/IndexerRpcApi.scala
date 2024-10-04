package xyz.stratalab.byzantine.util

import cats.effect._
import cats.implicits._
import xyz.stratalab.algebras.IndexerRpc

import fs2.Stream
import org.typelevel.log4cats.Logger
import scala.concurrent.duration._

class IndexerRpcApi[F[_]](val client: IndexerRpc[F]) extends AnyVal {

  def waitForRpcStartUp(implicit asyncF: Async[F], loggerF: Logger[F]): F[Unit] =
    for {
      _ <- Logger[F].info("Waiting for Indexer RPC to start up")
      _ <-
        Stream
          .retry(
            client
              .blockIdAtHeight(1),
            250.milli,
            identity,
            200
          )
          .compile
          .lastOrError
      _ <- Logger[F].info("Genesis block timestamp reached.  Node is ready.")
    } yield ()
}
