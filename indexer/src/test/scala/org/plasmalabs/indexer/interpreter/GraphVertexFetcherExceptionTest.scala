package org.plasmalabs.indexer.interpreter

import cats.effect.{IO, Resource, Sync}
import cats.implicits.*
import com.orientechnologies.orient.core.command.OCommandRequest
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.OrientGraphNoTx
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import org.plasmalabs.consensus.models.BlockHeader
import org.plasmalabs.indexer.model.{GE, GEs}
import org.plasmalabs.indexer.orientDb.OrientThread
import org.plasmalabs.models.generators.consensus.ModelGenerators.*
import org.plasmalabs.sdk.generators.ModelGenerators.*
import org.plasmalabs.sdk.models.{LockAddress, TransactionOutputAddress}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class GraphVertexFetcherExceptionTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  test("On fetchHeader with throwable response, a MessageWithCause should be returned") {

    val expectedTh = new IllegalStateException("boom!")

    PropF.forAllF { (header: BlockHeader) =>
      withMock {
        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          orientGraphNoTx       <- throwableOrientGraphResource(expectedTh)
          graphVertexFetcher    <- GraphVertexFetcher.make[F](orientGraphNoTx)
          _ <- assertIO(
            graphVertexFetcher.fetchHeader(header.id),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchHeader", expectedTh): GE)
              .asLeft[Option[Vertex]]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  test("On fetchHeaderByHeight with throwable response, a MessageWithCause should be returned") {

    PropF.forAllF { (height: Long) =>
      withMock {
        val expectedTh = new IllegalStateException("boom!")

        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          orientGraphNoTx       <- throwableOrientGraphResource(expectedTh)
          graphVertexFetcher    <- GraphVertexFetcher.make[F](orientGraphNoTx)
          _ <- assertIO(
            graphVertexFetcher.fetchHeaderByHeight(height),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchHeaderByHeight", expectedTh): GE)
              .asLeft[Option[Vertex]]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  test("On fetchBody with throwable response, a MessageWithCause should be returned") {

    val expectedTh = new IllegalStateException("boom!")

    withMock {
      val res = for {
        given OrientThread[F] <- OrientThread.create[F]
        orientGraphNoTx       <- throwableOrientGraphResource(expectedTh)
        vertex                <- mock[Vertex].pure[F].toResource
        graphVertexFetcher    <- GraphVertexFetcher.make[F](orientGraphNoTx)
        _ <- assertIO(
          graphVertexFetcher.fetchBody(vertex),
          (GEs.InternalMessageCause("GraphVertexFetcher:fetchBody", expectedTh): GE)
            .asLeft[Option[Vertex]]
        ).toResource
      } yield ()

      res.use_
    }

  }

  test("On fetchTransactions with throwable response, a MessageWithCause should be returned") {

    val expectedTh = new IllegalStateException("boom!")

    withMock {
      val res = for {
        given OrientThread[F] <- OrientThread.create[F]
        orientGraphNoTx       <- throwableOrientGraphResource(expectedTh)
        vertex                <- mock[Vertex].pure[F].toResource
        graphVertexFetcher    <- GraphVertexFetcher.make[F](orientGraphNoTx)
        _ <- assertIO(
          graphVertexFetcher.fetchTransactions(vertex),
          (GEs.InternalMessageCause("GraphVertexFetcher:fetchTransactions", expectedTh): GE)
            .asLeft[List[Vertex]]
        ).toResource
      } yield ()

      res.use_
    }

  }

  test("On fetchLockAddress with throwable response, a MessageWithCause should be returned") {

    val expectedTh = new IllegalStateException("boom!")

    PropF.forAllF { (lockAddress: LockAddress) =>
      withMock {
        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          orientGraphNoTx       <- throwableOrientGraphResource(expectedTh)
          graphVertexFetcher    <- GraphVertexFetcher.make[F](orientGraphNoTx)
          _ <- assertIO(
            graphVertexFetcher.fetchLockAddress(lockAddress),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchLockAddress", expectedTh): GE)
              .asLeft[Option[Vertex]]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  test("On fetchTxo with throwable response, a MessageWithCause should be returned") {

    val expectedTh = new IllegalStateException("boom!")

    PropF.forAllF { (transactionOutputAddress: TransactionOutputAddress) =>
      withMock {
        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          orientGraphNoTx       <- throwableOrientGraphResource(expectedTh)
          graphVertexFetcher    <- GraphVertexFetcher.make[F](orientGraphNoTx)
          _ <- assertIO(
            graphVertexFetcher.fetchTxo(transactionOutputAddress),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchTxo", expectedTh): GE)
              .asLeft[Option[Vertex]]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  private def throwableOrientGraphResource(throwable: => Throwable): Resource[IO, OrientGraphNoTx] =
    Resource.make(Sync[F].blocking(new OrientGraphNoTx("memory:test") {
      override def command(sql: OCommandRequest): OCommandRequest = throw throwable
    }))(g => Sync[F].delay(g.shutdown()))

}
