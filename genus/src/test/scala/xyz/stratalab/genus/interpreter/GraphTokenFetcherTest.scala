package xyz.stratalab.genus.interpreter

import cats.effect.IO
import cats.implicits._
import xyz.stratalab.sdk.generators.ModelGenerators._
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.{GroupId, SeriesId, TransactionOutputAddress}
import com.tinkerpop.blueprints.Vertex
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import xyz.stratalab.genus.algebras.VertexFetcherAlgebra
import xyz.stratalab.genus.interpreter.GraphTokenFetcher
import xyz.stratalab.genus.model.{GE, GEs}
import xyz.stratalab.genus.orientDb.instances.{SchemaGroupPolicy, SchemaSeriesPolicy}

class GraphTokenFetcherTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("On fetchGroupPolicy with throwable response, a FailureMessageWithCause should be returned") {

    PropF.forAllF { (groupId: GroupId) =>
      withMock {

        val res = for {
          vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          expectedTh = new IllegalStateException("boom!")
          _ = (vertexFetcher.fetchGroupPolicy)
            .expects(groupId)
            .once()
            .returning(
              (GEs
                .InternalMessageCause("GraphVertexFetcher:fetchGroupPolicy", expectedTh): GE)
                .asLeft[Option[Vertex]]
                .pure[F]
            )
          tokenFetcher <- GraphTokenFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            tokenFetcher.fetchGroupPolicy(groupId),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchGroupPolicy", expectedTh): GE)
              .asLeft[Option[GroupPolicy]]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchGroupPolicy if the Vertex exist, Some GroupPolicy should be returned") {

    PropF.forAllF { (groupId: GroupId, seriesId: SeriesId, address: TransactionOutputAddress) =>
      withMock {

        val res = for {
          vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          vertex        <- mock[Vertex].pure[F].toResource

          groupPolicy = GroupPolicy("fooboo", address, Some(seriesId))

          _ = (vertexFetcher.fetchGroupPolicy)
            .expects(groupId)
            .once()
            .returning(Option(vertex).asRight[GE].pure[F])

          _ = (vertex.getProperty[String])
            .expects(SchemaGroupPolicy.Field.Label)
            .once()
            .returning(groupPolicy.label)

          _ = (vertex.getProperty[Array[Byte]])
            .expects(SchemaGroupPolicy.Field.RegistrationUtxo)
            .once()
            .returning(groupPolicy.registrationUtxo.toByteArray)

          _ = (vertex.getProperty[Array[Byte]])
            .expects(SchemaGroupPolicy.Field.FixedSeries)
            .once()
            .returning(groupPolicy.fixedSeries.map(_.value.toByteArray).getOrElse(Array.empty[Byte]))

          tokenFetcher <- GraphTokenFetcher.make[F](vertexFetcher)
          _            <- assertIO(tokenFetcher.fetchGroupPolicy(groupId), Some(groupPolicy).asRight[GE]).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchSeriesPolicy with throwable response, a FailureMessageWithCause should be returned") {

    PropF.forAllF { (id: SeriesId) =>
      withMock {

        val res = for {
          vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          expectedTh = new IllegalStateException("boom!")
          _ = (vertexFetcher.fetchSeriesPolicy)
            .expects(id)
            .once()
            .returning(
              (GEs
                .InternalMessageCause("GraphVertexFetcher:fetchGroupPolicy", expectedTh): GE)
                .asLeft[Option[Vertex]]
                .pure[F]
            )
          tokenFetcher <- GraphTokenFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            tokenFetcher.fetchSeriesPolicy(id),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchGroupPolicy", expectedTh): GE)
              .asLeft[Option[SeriesPolicy]]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchSeriesPolicy if the Vertex exist, Some SeriesPolicy should be returned") {

    PropF.forAllF { (id: SeriesId, address: TransactionOutputAddress) =>
      withMock {

        val res = for {
          vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          vertex        <- mock[Vertex].pure[F].toResource

          seriesPolicy = SeriesPolicy(
            label = "fooboo",
            tokenSupply = Some(1),
            registrationUtxo = address
          )

          _ = (vertexFetcher.fetchSeriesPolicy)
            .expects(id)
            .once()
            .returning(Option(vertex).asRight[GE].pure[F])

          _ = (vertex.getProperty[String])
            .expects(SchemaSeriesPolicy.Field.Label)
            .once()
            .returning(seriesPolicy.label)

          _ = (vertex.getProperty[Int])
            .expects(SchemaSeriesPolicy.Field.TokenSupply)
            .once()
            .returning(seriesPolicy.tokenSupply.get)

          _ = (vertex.getProperty[Array[Byte]])
            .expects(SchemaSeriesPolicy.Field.RegistrationUtxo)
            .once()
            .returning(seriesPolicy.registrationUtxo.toByteArray)

          _ = (vertex.getProperty[Int])
            .expects(SchemaSeriesPolicy.Field.QuantityDescriptor)
            .once()
            .returning(seriesPolicy.quantityDescriptor.value)

          _ = (vertex.getProperty[Int])
            .expects(SchemaSeriesPolicy.Field.Fungibility)
            .once()
            .returning(seriesPolicy.fungibility.value)

          _ = (vertex.getProperty[Array[Byte]])
            .expects(SchemaSeriesPolicy.Field.EphemeralMetadataScheme)
            .once()
            .returning(seriesPolicy.ephemeralMetadataScheme.map(_.toByteArray).getOrElse(Array.empty[Byte]))

          _ = (vertex.getProperty[Array[Byte]])
            .expects(SchemaSeriesPolicy.Field.PermanentMetadataScheme)
            .once()
            .returning(seriesPolicy.permanentMetadataScheme.map(_.toByteArray).getOrElse(Array.empty[Byte]))

          tokenFetcher <- GraphTokenFetcher.make[F](vertexFetcher)
          _            <- assertIO(tokenFetcher.fetchSeriesPolicy(id), Some(seriesPolicy).asRight[GE]).toResource
        } yield ()

        res.use_
      }

    }
  }

}
