package xyz.stratalab.indexer

import cats.effect.IO
import cats.implicits._
import io.grpc.{Metadata, StatusException}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import xyz.stratalab.indexer.algebras.TokenFetcherAlgebra
import xyz.stratalab.indexer.model.{GE, GEs}
import xyz.stratalab.indexer.services._
import xyz.stratalab.sdk.generators.ModelGenerators._
import xyz.stratalab.sdk.models.Event.{GroupPolicy, SeriesPolicy}
import xyz.stratalab.sdk.models.{GroupId, SeriesId, TransactionOutputAddress}

class GrpcTokenServiceTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("getGroupPolicy: Exceptions") {
    PropF.forAllF { (id: GroupId) =>
      withMock {
        val fetcher = mock[TokenFetcherAlgebra[F]]
        val underTest = new GrpcTokenService[F](fetcher)

        (fetcher.fetchGroupPolicy _)
          .expects(id)
          .once()
          .returning((GEs.Internal(new IllegalStateException("Boom!")): GE).asLeft[Option[GroupPolicy]].pure[F])

        for {
          _ <- interceptMessageIO[StatusException]("INTERNAL: Boom!")(
            underTest.getGroupPolicy(QueryByGroupIdRequest(id), new Metadata())
          )
        } yield ()
      }
    }

  }

  test("getGroupPolicy: Not Found") {
    PropF.forAllF { (id: GroupId) =>
      withMock {
        val fetcher = mock[TokenFetcherAlgebra[F]]
        val underTest = new GrpcTokenService[F](fetcher)

        (fetcher.fetchGroupPolicy _)
          .expects(id)
          .once()
          .returning(Option.empty[GroupPolicy].asRight[GE].pure[F])

        for {
          res <- underTest.getGroupPolicy(QueryByGroupIdRequest(id), new Metadata())
          _ = assert(res.groupPolicy.isEmpty)
        } yield ()
      }
    }

  }

  test("getGroupPolicy: Ok") {
    PropF.forAllF { (id: GroupId, seriesId: SeriesId, address: TransactionOutputAddress) =>
      withMock {
        val fetcher = mock[TokenFetcherAlgebra[F]]
        val underTest = new GrpcTokenService[F](fetcher)

        val groupPolicy = GroupPolicy("fooboo", address, Some(seriesId))

        (fetcher.fetchGroupPolicy _)
          .expects(id)
          .once()
          .returning(groupPolicy.some.asRight[GE].pure[F])

        for {
          res <- underTest.getGroupPolicy(QueryByGroupIdRequest(id), new Metadata())
          _ = assert(res.groupPolicy.get == groupPolicy)
        } yield ()
      }
    }

  }

  test("getSeriesPolicy: Exceptions") {
    PropF.forAllF { (id: SeriesId) =>
      withMock {
        val fetcher = mock[TokenFetcherAlgebra[F]]
        val underTest = new GrpcTokenService[F](fetcher)

        (fetcher.fetchSeriesPolicy _)
          .expects(id)
          .once()
          .returning((GEs.Internal(new IllegalStateException("Boom!")): GE).asLeft[Option[SeriesPolicy]].pure[F])

        for {
          _ <- interceptMessageIO[StatusException]("INTERNAL: Boom!")(
            underTest.getSeriesPolicy(QueryBySeriesIdRequest(id), new Metadata())
          )
        } yield ()
      }
    }

  }

  test("getSeriesPolicy: Not Found") {
    PropF.forAllF { (id: SeriesId) =>
      withMock {
        val fetcher = mock[TokenFetcherAlgebra[F]]
        val underTest = new GrpcTokenService[F](fetcher)

        (fetcher.fetchSeriesPolicy _)
          .expects(id)
          .once()
          .returning(Option.empty[SeriesPolicy].asRight[GE].pure[F])

        for {
          res <- underTest.getSeriesPolicy(QueryBySeriesIdRequest(id), new Metadata())
          _ = assert(res.seriesPolicy.isEmpty)
        } yield ()
      }
    }

  }

  test("getSeriesPolicy: Ok") {
    PropF.forAllF { (id: SeriesId, address: TransactionOutputAddress) =>
      withMock {
        val fetcher = mock[TokenFetcherAlgebra[F]]
        val underTest = new GrpcTokenService[F](fetcher)

        val seriesPolicy = SeriesPolicy(
          label = "fooboo",
          tokenSupply = Some(1),
          registrationUtxo = address
        )

        (fetcher.fetchSeriesPolicy _)
          .expects(id)
          .once()
          .returning(seriesPolicy.some.asRight[GE].pure[F])

        for {
          res <- underTest.getSeriesPolicy(QueryBySeriesIdRequest(id), new Metadata())
          _ = assert(res.seriesPolicy.get == seriesPolicy)
        } yield ()
      }
    }

  }

}
