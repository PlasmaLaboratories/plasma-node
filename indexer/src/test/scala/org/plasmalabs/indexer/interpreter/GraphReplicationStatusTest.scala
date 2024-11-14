package org.plasmalabs.indexer.interpreter

import cats.effect.IO
import cats.implicits._
import com.tinkerpop.blueprints.Vertex
import fs2.Stream
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import org.plasmalabs.consensus.models.BlockHeader
import org.plasmalabs.indexer.algebras.{NodeBlockFetcherAlgebra, VertexFetcherAlgebra}
import org.plasmalabs.indexer.model.{GE, GEs}
import org.plasmalabs.indexer.orientDb.OrientThread
import org.plasmalabs.indexer.orientDb.instances.SchemaBlockHeader
import org.plasmalabs.models.generators.consensus.ModelGenerators._
import org.plasmalabs.typeclasses.implicits.showBlockId
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import scala.concurrent.duration.DurationInt

class GraphReplicationStatusTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]
  type S[A] = Stream[F, A]

  test("On canonicalHeadSynced with throwable response, a InternalMessageCause should be returned") {

    withMock {

      val res = for {
        given OrientThread[F]   <- OrientThread.create[F]
        graphVertexFetcher      <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
        nodeBlockFetcherAlgebra <- mock[NodeBlockFetcherAlgebra[F, S]].pure[F].toResource
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
        given OrientThread[F]   <- OrientThread.create[F]
        graphVertexFetcher      <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
        nodeBlockFetcherAlgebra <- mock[NodeBlockFetcherAlgebra[F, S]].pure[F].toResource
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
        blockHeaderIndexer: BlockHeader,
        blockHeaderNode:    BlockHeader
      ) =>
        withMock {
          val res = for {
            given OrientThread[F]   <- OrientThread.create[F]
            graphVertexFetcher      <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
            nodeBlockFetcherAlgebra <- mock[NodeBlockFetcherAlgebra[F, S]].pure[F].toResource
            blockHeaderVertex       <- mock[Vertex].pure[F].toResource

            // vertex properties mocks
            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.BlockId)
              .once()
              .returning(blockHeaderIndexer.embedId.id.value.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[java.lang.Long])
              .expects(SchemaBlockHeader.Field.ParentSlot)
              .once()
              .returning(blockHeaderIndexer.parentSlot)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.EligibilityCertificate)
              .once()
              .returning(blockHeaderIndexer.eligibilityCertificate.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.OperationalCertificate)
              .once()
              .returning(blockHeaderIndexer.operationalCertificate.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.Address)
              .once()
              .returning(blockHeaderIndexer.address.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.Metadata)
              .once()
              .returning(blockHeaderIndexer.metadata.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.TxRoot)
              .once()
              .returning(blockHeaderIndexer.txRoot.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.BloomFilter)
              .once()
              .returning(blockHeaderIndexer.bloomFilter.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.ParentHeaderId)
              .once()
              .returning(blockHeaderIndexer.parentHeaderId.value.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[java.lang.Long])
              .expects(SchemaBlockHeader.Field.Slot)
              .once()
              .returning(blockHeaderIndexer.slot)

            _ = (blockHeaderVertex
              .getProperty[java.lang.Long])
              .expects(SchemaBlockHeader.Field.Height)
              .once()
              .returning(blockHeaderIndexer.height)

            _ = (blockHeaderVertex
              .getProperty[java.lang.Long])
              .expects(SchemaBlockHeader.Field.Timestamp)
              .once()
              .returning(blockHeaderIndexer.timestamp)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.Version)
              .once()
              .returning(blockHeaderIndexer.version.toByteArray)

            // end vertex properties mocks

            _ = (() => graphVertexFetcher.fetchCanonicalHead())
              .expects()
              .returning(blockHeaderVertex.some.asRight[GE].pure[F])

            _ = (() => nodeBlockFetcherAlgebra.fetchCanonicalHeadId())
              .expects()
              .returning(blockHeaderNode.id.some.pure[F])

            graphReplicatorStatus <- GraphReplicationStatus
              .make[F](graphVertexFetcher, nodeBlockFetcherAlgebra, 1.minute)

            _ <- assertIO(
              graphReplicatorStatus.canonicalHeadSynced.map(_.left.map(_.getMessage)),
              (show"Indexer canonical head [${blockHeaderIndexer.id}] differs to Node [${blockHeaderNode.id}]")
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
            given OrientThread[F]   <- OrientThread.create[F]
            graphVertexFetcher      <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
            nodeBlockFetcherAlgebra <- mock[NodeBlockFetcherAlgebra[F, S]].pure[F].toResource
            blockHeaderVertex       <- mock[Vertex].pure[F].toResource

            // vertex properties mocks
            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.BlockId)
              .once()
              .returning(blockHeader.embedId.id.value.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[java.lang.Long])
              .expects(SchemaBlockHeader.Field.ParentSlot)
              .once()
              .returning(blockHeader.parentSlot)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.EligibilityCertificate)
              .once()
              .returning(blockHeader.eligibilityCertificate.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.OperationalCertificate)
              .once()
              .returning(blockHeader.operationalCertificate.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.Address)
              .once()
              .returning(blockHeader.address.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.Metadata)
              .once()
              .returning(blockHeader.metadata.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.TxRoot)
              .once()
              .returning(blockHeader.txRoot.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.BloomFilter)
              .once()
              .returning(blockHeader.bloomFilter.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.ParentHeaderId)
              .once()
              .returning(blockHeader.parentHeaderId.value.toByteArray)

            _ = (blockHeaderVertex
              .getProperty[java.lang.Long])
              .expects(SchemaBlockHeader.Field.Slot)
              .once()
              .returning(blockHeader.slot)

            _ = (blockHeaderVertex
              .getProperty[java.lang.Long])
              .expects(SchemaBlockHeader.Field.Height)
              .once()
              .returning(blockHeader.height)

            _ = (blockHeaderVertex
              .getProperty[java.lang.Long])
              .expects(SchemaBlockHeader.Field.Timestamp)
              .once()
              .returning(blockHeader.timestamp)

            _ = (blockHeaderVertex
              .getProperty[Array[Byte]])
              .expects(SchemaBlockHeader.Field.Version)
              .once()
              .returning(blockHeader.version.toByteArray)

            // end vertex properties mocks

            _ = (() => graphVertexFetcher.fetchCanonicalHead())
              .expects()
              .returning(blockHeaderVertex.some.asRight[GE].pure[F])

            _ = (() => nodeBlockFetcherAlgebra.fetchCanonicalHeadId())
              .expects()
              .returning(blockHeader.id.some.pure[F])

            graphReplicatorStatus <- GraphReplicationStatus
              .make[F](graphVertexFetcher, nodeBlockFetcherAlgebra, 1.minute)

            _ <- assertIO(graphReplicatorStatus.canonicalHeadSynced, true.asRight[GE]).toResource
          } yield ()
          res.use_

        }
    }
  }

}
