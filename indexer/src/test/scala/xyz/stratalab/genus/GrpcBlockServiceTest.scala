package xyz.stratalab.indexer

import cats.effect.IO
import cats.implicits._
import io.grpc.{Metadata, StatusException}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import xyz.stratalab.consensus.models._
import xyz.stratalab.indexer.algebras.{BlockFetcherAlgebra, GraphReplicationStatusAlgebra}
import xyz.stratalab.indexer.model.{GE, GEs}
import xyz.stratalab.indexer.services._
import xyz.stratalab.models.generators.consensus.ModelGenerators._
import xyz.stratalab.models.generators.node.ModelGenerators._
import xyz.stratalab.node.models._
import xyz.stratalab.typeclasses.implicits._

class GrpcBlockServiceTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("getBlockById: OK") {
    PropF.forAllF { (blockId: BlockId, blockHeader: BlockHeader, blockBody: FullBlockBody) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val replicatorStatus = mock[GraphReplicationStatusAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher, replicatorStatus)

        val blockData = BlockData(blockHeader, blockBody)

        (() => replicatorStatus.canonicalHeadSynced).expects().once().returning(true.asRight[GE].pure[F])

        (blockFetcher.fetchBlock _).expects(blockId).once().returning(blockData.some.asRight[GE].pure[F])

        for {
          res <- underTest.getBlockById(GetBlockByIdRequest(blockId), new Metadata())
          _ = assert(
            res == BlockResponse(
              FullBlock(blockData.header, blockData.body)
            )
          )
        } yield ()
      }
    }
  }

  test("getBlockById: Not Found") {
    PropF.forAllF { (blockId: BlockId) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val replicatorStatus = mock[GraphReplicationStatusAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher, replicatorStatus)

        (blockFetcher.fetchBlock _)
          .expects(blockId)
          .once()
          .returning(Option.empty[BlockData].asRight[GE].pure[F])

        (() => replicatorStatus.canonicalHeadSynced).expects().once().returning(true.asRight[GE].pure[F])

        for {
          _ <- interceptMessageIO[StatusException](s"NOT_FOUND: BlockId:${blockId.show}")(
            underTest.getBlockById(GetBlockByIdRequest(blockId), new Metadata())
          )
        } yield ()
      }
    }

  }

  test("getBlockById: Exceptions") {
    PropF.forAllF { (blockId: BlockId) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val replicatorStatus = mock[GraphReplicationStatusAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher, replicatorStatus)

        (blockFetcher.fetchBlock _)
          .expects(blockId)
          .once()
          .returning((GEs.Internal(new IllegalStateException("Boom!")): GE).asLeft[Option[BlockData]].pure[F])

        (() => replicatorStatus.canonicalHeadSynced).expects().once().returning(true.asRight[GE].pure[F])

        for {
          _ <- interceptMessageIO[StatusException]("INTERNAL: Boom!")(
            underTest.getBlockById(GetBlockByIdRequest(blockId), new Metadata())
          )
        } yield ()
      }
    }

  }

  test("getBlockByHeight: OK") {
    PropF.forAllF { (height: Long, blockHeader: BlockHeader, blockBody: FullBlockBody) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val replicatorStatus = mock[GraphReplicationStatusAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher, replicatorStatus)

        val blockData = BlockData(blockHeader, blockBody)

        (blockFetcher.fetchBlockByHeight _)
          .expects(height)
          .once()
          .returning(blockData.some.asRight[GE].pure[F])

        (() => replicatorStatus.canonicalHeadSynced).expects().once().returning(true.asRight[GE].pure[F])

        for {
          res <- underTest.getBlockByHeight(GetBlockByHeightRequest(ChainDistance(height)), new Metadata())
          _ = assert(res == BlockResponse(FullBlock(blockData.header, blockData.body)))
        } yield ()
      }
    }
  }

  test("getBlockByHeight: Not Found") {
    PropF.forAllF { (height: Long) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val replicatorStatus = mock[GraphReplicationStatusAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher, replicatorStatus)

        (blockFetcher.fetchBlockByHeight _)
          .expects(height)
          .once()
          .returning(Option.empty[BlockData].asRight[GE].pure[F])

        (() => replicatorStatus.canonicalHeadSynced).expects().once().returning(true.asRight[GE].pure[F])

        for {
          _ <- interceptMessageIO[StatusException](s"NOT_FOUND: Height:${height.show}")(
            underTest.getBlockByHeight(GetBlockByHeightRequest(ChainDistance(height)), new Metadata())
          )
        } yield ()
      }
    }

  }

  test("getBlockByHeight: Exceptions") {
    PropF.forAllF { (height: Long) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val replicatorStatus = mock[GraphReplicationStatusAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher, replicatorStatus)

        (blockFetcher.fetchBlockByHeight _)
          .expects(height)
          .once()
          .returning((GEs.UnImplemented: GE).asLeft[Option[BlockData]].pure[F])

        (() => replicatorStatus.canonicalHeadSynced).expects().once().returning(true.asRight[GE].pure[F])

        for {
          _ <- interceptMessageIO[StatusException]("INTERNAL: An implementation is missing")(
            underTest.getBlockByHeight(GetBlockByHeightRequest(ChainDistance(height)), new Metadata())
          )
        } yield ()
      }
    }

  }

  test("getBlockByDepth: OK") {
    PropF.forAllF { (depth: Long, blockHeader: BlockHeader, blockBody: FullBlockBody) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val replicatorStatus = mock[GraphReplicationStatusAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher, replicatorStatus)

        val blockData = BlockData(blockHeader, blockBody)

        (blockFetcher.fetchBlockByDepth _)
          .expects(depth)
          .once()
          .returning(blockData.some.asRight[GE].pure[F])

        (() => replicatorStatus.canonicalHeadSynced).expects().once().returning(true.asRight[GE].pure[F])

        for {
          res <- underTest.getBlockByDepth(GetBlockByDepthRequest(ChainDistance(depth)), new Metadata())
          _ = assert(
            res == BlockResponse(FullBlock(blockData.header, blockData.body))
          )
        } yield ()
      }
    }
  }

  test("getBlockByDepth: Not Found") {
    PropF.forAllF { (depth: Long) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val replicatorStatus = mock[GraphReplicationStatusAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher, replicatorStatus)

        (blockFetcher.fetchBlockByDepth _)
          .expects(depth)
          .once()
          .returning(Option.empty[BlockData].asRight[GE].pure[F])

        (() => replicatorStatus.canonicalHeadSynced).expects().once().returning(true.asRight[GE].pure[F])

        for {
          _ <- interceptMessageIO[StatusException](s"NOT_FOUND: Depth:${depth.show}")(
            underTest.getBlockByDepth(GetBlockByDepthRequest(ChainDistance(depth)), new Metadata())
          )
        } yield ()
      }
    }

  }

  test("getBlockByDepth: Exceptions") {
    PropF.forAllF { (depth: Long) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val replicatorStatus = mock[GraphReplicationStatusAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher, replicatorStatus)

        (blockFetcher.fetchBlockByDepth _)
          .expects(depth)
          .once()
          .returning((GEs.InternalMessage("Boom!"): GE).asLeft[Option[BlockData]].pure[F])

        (() => replicatorStatus.canonicalHeadSynced).expects().once().returning(true.asRight[GE].pure[F])

        for {
          _ <- interceptMessageIO[StatusException]("INTERNAL: Boom!")(
            underTest.getBlockByDepth(GetBlockByDepthRequest(ChainDistance(depth)), new Metadata())
          )
        } yield ()
      }
    }

  }

}
