package org.plasmalabs.indexer.interpreter

import cats.implicits._
import fs2.Stream
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import org.plasmalabs.consensus.models.{BlockHeader, BlockId}
import org.plasmalabs.indexer.DbFixtureUtil
import org.plasmalabs.indexer.algebras.{BlockFetcherAlgebra, NodeBlockFetcherAlgebra}
import org.plasmalabs.indexer.model.{GE, GEs}
import org.plasmalabs.indexer.orientDb.OrientThread
import org.plasmalabs.indexer.services.BlockData
import org.plasmalabs.models.generators.consensus.ModelGenerators._
import org.plasmalabs.node.models.FullBlockBody
import org.plasmalabs.typeclasses.implicits._
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class GraphBlockUpdaterTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with DbFixtureUtil {

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMaxSize(3)
      .withMinSuccessfulTests(5)

//

  orientDbFixtureNew.test("Insert no genesis block, should fail, if we can not fetch block") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      PropF.forAllF { blockHeader: BlockHeader =>
        withMock {
          val nodeBlockFetcher = mock[NodeBlockFetcherAlgebra[F, Stream[F, *]]]
          val blockFetcher = mock[BlockFetcherAlgebra[F]]

          val blockData = BlockData(blockHeader.copy(height = 2), FullBlockBody(Seq.empty, None))

          (blockFetcher.fetchBlock _)
            .expects(blockData.header.parentHeaderId)
            .returning((GEs.InternalMessage("boom!"): GE).asLeft[Option[BlockData]].pure[F])
            .once()

          val res = for {
            dbTx              <- oThread.delay(odbFactory.getTx).toResource
            graphBlockUpdater <- GraphBlockUpdater.make[F](dbTx, blockFetcher, nodeBlockFetcher)

            _ <- assertIO(
              graphBlockUpdater.insert(blockData),
              (GEs.InternalMessage("boom!"): GE).asLeft[Unit]
            ).toResource
          } yield ()
          res.use_
        }
      }
  }

  orientDbFixtureNew.test("Insert no genesis block, should fail, if we can not find the parent vertex") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      PropF.forAllF { blockHeader: BlockHeader =>
        withMock {
          val nodeBlockFetcher = mock[NodeBlockFetcherAlgebra[F, Stream[F, *]]]
          val blockFetcher = mock[BlockFetcherAlgebra[F]]
          val blockHeaderWithId = blockHeader.embedId
          val blockData = BlockData(blockHeaderWithId.copy(height = 2), FullBlockBody(Seq.empty, None))
          val blockDataParent = BlockData(blockHeaderWithId.copy(height = 1), FullBlockBody(Seq.empty, None))

          (blockFetcher.fetchBlock _)
            .expects(blockData.header.parentHeaderId)
            .returning(blockDataParent.some.asRight[GE].pure[F])
            .once()

          val res = for {
            dbTx              <- oThread.delay(odbFactory.getTx).toResource
            graphBlockUpdater <- GraphBlockUpdater.make[F](dbTx, blockFetcher, nodeBlockFetcher)

            _ <- assertIO(
              graphBlockUpdater.insert(blockData),
              (GEs.InternalMessage(show"Parent header vertex not found ${blockData.header.parentHeaderId}"): GE)
                .asLeft[Unit]
            ).toResource
          } yield ()
          res.use_
        }
      }
  }

  orientDbFixtureNew.test("Insert genesis block, should work") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      PropF.forAllF { blockHeader: BlockHeader =>
        withMock {
          val nodeBlockFetcher = mock[NodeBlockFetcherAlgebra[F, Stream[F, *]]]
          val blockFetcher = mock[BlockFetcherAlgebra[F]]
          val res = for {
            dbTx <- oThread.delay(odbFactory.getTx).toResource
            blockData = BlockData(blockHeader.copy(height = 1), FullBlockBody(Seq.empty, None))
            graphBlockUpdater <- GraphBlockUpdater.make[F](dbTx, blockFetcher, nodeBlockFetcher)
            _ <- assertIO(
              graphBlockUpdater.insert(blockData),
              ().asRight[GE]
            ).toResource
          } yield ()

          res.use_

        }
      }

  }

  orientDbFixtureNew.test("Insert no genesis block, should fail, if we can not fech parent block from node") {
    case (odbFactory, implicit0(oThread: OrientThread[F])) =>
      PropF.forAllF { blockHeader: BlockHeader =>
        withMock {
          val nodeBlockFetcher = mock[NodeBlockFetcherAlgebra[F, Stream[F, *]]]
          val blockFetcher = mock[BlockFetcherAlgebra[F]]

          val blockHeaderWithId = blockHeader.embedId
          val blockData = BlockData(blockHeaderWithId.copy(height = 2), FullBlockBody(Seq.empty, None))

          (blockFetcher.fetchBlock _)
            .expects(blockData.header.parentHeaderId)
            .returning(Option.empty.asRight[GE].pure[F])
            .once()

          (nodeBlockFetcher
            .fetch(_: BlockId))
            .expects(blockData.header.parentHeaderId)
            .returning((GEs.InternalMessage("boom!"): GE).asLeft[BlockData].pure[F])
            .once()

          val res = for {
            dbTx              <- oThread.delay(odbFactory.getTx).toResource
            graphBlockUpdater <- GraphBlockUpdater.make[F](dbTx, blockFetcher, nodeBlockFetcher)
            _ <- assertIO(
              graphBlockUpdater.insert(blockData),
              (GEs.InternalMessage("boom!"): GE).asLeft[Unit]
            ).toResource
          } yield ()
          res.use_

        }
      }

  }
}
