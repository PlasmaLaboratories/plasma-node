package xyz.stratalab.genus.interpreter

import cats.effect.IO
import cats.implicits._
import com.tinkerpop.blueprints.Vertex
import fs2.Stream
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import xyz.stratalab.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import xyz.stratalab.consensus.models.BlockHeader
import xyz.stratalab.indexer.algebras.{NodeBlockFetcherAlgebra, VertexFetcherAlgebra}
import xyz.stratalab.indexer.interpreter.GraphReplicationStatus
import xyz.stratalab.indexer.model.{GE, GEs}
import xyz.stratalab.indexer.orientDb.OrientThread
import xyz.stratalab.indexer.orientDb.instances.SchemaBlockHeader
import xyz.stratalab.models.generators.consensus.ModelGenerators._

import scala.concurrent.duration.DurationInt

class GraphReplicationStatusTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]
  type S[A] = Stream[F, A]

  test("On canonicalHeadSynced with throwable response, a InternalMessageCause should be returned") {

    withMock {

      val res = for {
        implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
        graphVertexFetcher                       <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
        nodeBlockFetcherAlgebra                  <- mock[NodeBlockFetcherAlgebra[F, S]].pure[F].toResource
        expectedTh = new IllegalStateException("boom!")
        _ = (() => graphVertexFetcher.fetchCanonicalHead())
          .expects()
          .once()
          .returning(
            (GEs
              .InternalMessageCause("GraphVertexFetcher:fetchCanonicalHead", expectedTh): GE)
              .asLeft[Option[Vertex]]
              .pure[F]
          )
        graphReplicatorStatus <- GraphReplicationStatus.make[F](graphVertexFetcher, nodeBlockFetcherAlgebra, 1.minute)
        _ <- assertIO(
          graphReplicatorStatus.canonicalHeadSynced,
          (GEs.InternalMessageCause("GraphVertexFetcher:fetchCanonicalHead", expectedTh): GE)
            .asLeft[Boolean]
        ).toResource
      } yield ()

      res.use_
    }
  }

  test("On canonicalHeadSynced if an empty iterator is returned, None BlockHeader should be returned") {
    withMock {
      val res = for {
        implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
        graphVertexFetcher                       <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
        nodeBlockFetcherAlgebra                  <- mock[NodeBlockFetcherAlgebra[F, S]].pure[F].toResource
        _ = (() => graphVertexFetcher.fetchCanonicalHead())
          .expects()
          .returning(Option.empty[Vertex].asRight[GE].pure[F])
        graphReplicatorStatus <- GraphReplicationStatus.make[F](graphVertexFetcher, nodeBlockFetcherAlgebra, 1.minute)

        _ <- assertIO(
          graphReplicatorStatus.canonicalHeadSynced.map(_.left.map(_.getMessage)),
          ("Indexer empty response at fetchCanonicalHead").asLeft[Boolean]
        ).toResource
      } yield ()
      res.use_
    }
  }

  test(
    "On canonicalHeadSynced if vertex is returned, but is not equal to node canonical head, a Internal Error should be returned"
  ) {

    PropF.forAllF {
      (
        blockHeader: BlockHeader
      ) =>
        withMock {
          val res = for {
            implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
            graphVertexFetcher                       <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
            nodeBlockFetcherAlgebra                  <- mock[NodeBlockFetcherAlgebra[F, S]].pure[F].toResource
            blockHeaderVertex                        <- mock[Vertex].pure[F].toResource

            // vertex properties mocks
            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.BlockId)
              .once()
              .returning(blockHeader.embedId.id.value.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[java.lang.Long] _)
              .expects(SchemaBlockHeader.Field.ParentSlot)
              .once()
              .returning(blockHeader.parentSlot)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.EligibilityCertificate)
              .once()
              .returning(blockHeader.eligibilityCertificate.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.OperationalCertificate)
              .once()
              .returning(blockHeader.operationalCertificate.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.Address)
              .once()
              .returning(blockHeader.address.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.Metadata)
              .once()
              .returning(blockHeader.metadata.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.TxRoot)
              .once()
              .returning(blockHeader.txRoot.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.BloomFilter)
              .once()
              .returning(blockHeader.bloomFilter.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.ParentHeaderId)
              .once()
              .returning(blockHeader.parentHeaderId.value.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[java.lang.Long] _)
              .expects(SchemaBlockHeader.Field.Slot)
              .once()
              .returning(blockHeader.slot)

            _ = (blockHeaderVertex
              .getProperty[java.lang.Long] _)
              .expects(SchemaBlockHeader.Field.Height)
              .once()
              .returning(blockHeader.height)

            _ = (blockHeaderVertex
              .getProperty[java.lang.Long] _)
              .expects(SchemaBlockHeader.Field.Timestamp)
              .once()
              .returning(blockHeader.timestamp)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.Version)
              .once()
              .returning(blockHeader.version.toByteArray)

            // end vertex properties mocks

            _ = (() => graphVertexFetcher.fetchCanonicalHead())
              .expects()
              .returning(blockHeaderVertex.some.asRight[GE].pure[F])

            // Generator is defined with Gen.chooseNum(0L, 20L), height = 100 will create the expectation
            _ = (() => nodeBlockFetcherAlgebra.fetchHeight())
              .expects()
              .returning(100L.some.pure[F])

            graphReplicatorStatus <- GraphReplicationStatus
              .make[F](graphVertexFetcher, nodeBlockFetcherAlgebra, 1.minute)

            _ <- assertIO(
              graphReplicatorStatus.canonicalHeadSynced.map(_.left.map(_.getMessage)),
              (s"Indexer canonical head height:[${blockHeader.height}] differs to Node head[100]")
                .asLeft[Boolean]
            ).toResource
          } yield ()
          res.use_

        }
    }
  }

  test(
    "On canonicalHeadSynced if vertex is returned, and is equal to node canonical head"
  ) {

    PropF.forAllF {
      (
        blockHeader: BlockHeader
      ) =>
        withMock {
          val res = for {
            implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
            graphVertexFetcher                       <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
            nodeBlockFetcherAlgebra                  <- mock[NodeBlockFetcherAlgebra[F, S]].pure[F].toResource
            blockHeaderVertex                        <- mock[Vertex].pure[F].toResource

            // vertex properties mocks
            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.BlockId)
              .once()
              .returning(blockHeader.embedId.id.value.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[java.lang.Long] _)
              .expects(SchemaBlockHeader.Field.ParentSlot)
              .once()
              .returning(blockHeader.parentSlot)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.EligibilityCertificate)
              .once()
              .returning(blockHeader.eligibilityCertificate.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.OperationalCertificate)
              .once()
              .returning(blockHeader.operationalCertificate.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.Address)
              .once()
              .returning(blockHeader.address.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.Metadata)
              .once()
              .returning(blockHeader.metadata.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.TxRoot)
              .once()
              .returning(blockHeader.txRoot.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.BloomFilter)
              .once()
              .returning(blockHeader.bloomFilter.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.ParentHeaderId)
              .once()
              .returning(blockHeader.parentHeaderId.value.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[java.lang.Long] _)
              .expects(SchemaBlockHeader.Field.Slot)
              .once()
              .returning(blockHeader.slot)

            _ = (blockHeaderVertex
              .getProperty[java.lang.Long] _)
              .expects(SchemaBlockHeader.Field.Height)
              .once()
              .returning(blockHeader.height)

            _ = (blockHeaderVertex
              .getProperty[java.lang.Long] _)
              .expects(SchemaBlockHeader.Field.Timestamp)
              .once()
              .returning(blockHeader.timestamp)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]] _)
              .expects(SchemaBlockHeader.Field.Version)
              .once()
              .returning(blockHeader.version.toByteArray)

            // end vertex properties mocks

            _ = (() => graphVertexFetcher.fetchCanonicalHead())
              .expects()
              .returning(blockHeaderVertex.some.asRight[GE].pure[F])

            // Generator is defined with Gen.chooseNum(0L, 20L), height = 100 will create the expectation
            _ = (() => nodeBlockFetcherAlgebra.fetchHeight())
              .expects()
              .returning(blockHeader.height.some.pure[F])

            graphReplicatorStatus <- GraphReplicationStatus
              .make[F](graphVertexFetcher, nodeBlockFetcherAlgebra, 1.minute)

            _ <- assertIO(graphReplicatorStatus.canonicalHeadSynced, true.asRight[GE]).toResource
          } yield ()
          res.use_

        }
    }
  }

}
