package org.plasmalabs.grpc

import cats.Applicative
import cats.effect.IO
import cats.implicits._
import fs2.Stream
import io.grpc.Metadata
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.algebras.NodeRpc
import org.plasmalabs.codecs.bytes.tetra.instances._
import org.plasmalabs.consensus.models.{BlockHeader, BlockId}
import org.plasmalabs.models.generators.consensus.ModelGenerators._
import org.plasmalabs.models.generators.node.ModelGenerators._
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.node.services._
import org.plasmalabs.proto.node.{EpochData, NodeConfig}
import org.plasmalabs.sdk.generators.ModelGenerators._
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.syntax._
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class NodeGrpcSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("A transaction can be broadcast") {
    PropF.forAllF { (transaction: IoTransaction) =>
      withMock {
        val interpreter = mock[NodeRpc[F, Stream[F, *]]]
        val underTest = new NodeGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.broadcastTransaction)
          .expects(transaction)
          .once()
          .returning(Applicative[F].unit)

        for {
          res <- underTest.broadcastTransaction(BroadcastTransactionReq(transaction), new Metadata())
          _ = assert(res == BroadcastTransactionRes())
        } yield ()
      }
    }
  }

  test("A block header can be retrieved") {
    PropF.forAllF { (header: BlockHeader) =>
      val headerId = header.id
      withMock {
        val interpreter = mock[NodeRpc[F, Stream[F, *]]]
        val underTest = new NodeGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.fetchBlockHeader)
          .expects(headerId)
          .once()
          .returning(header.some.pure[F])

        for {
          res <- underTest.fetchBlockHeader(FetchBlockHeaderReq(headerId), new Metadata())
          _ = assert(res.header.get == header)
        } yield ()
      }
    }
  }

  test("A block body can be retrieved") {
    PropF.forAllF { (id: BlockId, body: BlockBody) =>
      withMock {
        val interpreter = mock[NodeRpc[F, Stream[F, *]]]
        val underTest = new NodeGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.fetchBlockBody)
          .expects(id)
          .once()
          .returning(body.some.pure[F])

        for {
          res <- underTest.fetchBlockBody(FetchBlockBodyReq(id), new Metadata())

          protoBody = res.body.get
          _ = assert(protoBody == body)
        } yield ()
      }
    }
  }

  test("A transaction can be retrieved") {
    PropF.forAllF { (transaction: IoTransaction) =>
      val transactionId = transaction.id
      withMock {
        val interpreter = mock[NodeRpc[F, Stream[F, *]]]
        val underTest = new NodeGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.fetchTransaction)
          .expects(transactionId)
          .once()
          .returning(transaction.some.pure[F])

        for {
          res <- underTest.fetchTransaction(FetchTransactionReq(transactionId), new Metadata())
          _ = assert(transaction == res.transaction.get)
        } yield ()
      }
    }
  }

  test("The block ID at a height can be retrieved") {
    PropF.forAllF { (height: Long, header: BlockHeader) =>
      val blockId = header.id
      withMock {
        val interpreter = mock[NodeRpc[F, Stream[F, *]]]
        val underTest = new NodeGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.blockIdAtHeight)
          .expects(height)
          .once()
          .returning(blockId.some.pure[F])

        for {
          res <- underTest.fetchBlockIdAtHeight(FetchBlockIdAtHeightReq(height), new Metadata())
          proto = res.blockId.get
          _ = assert(blockId == proto)
        } yield ()
      }
    }
  }

  test("Current Mempool Contains transactionId can be retrieved") {
    PropF.forAllF { (transaction: IoTransaction) =>
      val transactionId = transaction.id
      withMock {
        val interpreter = mock[NodeRpc[F, Stream[F, *]]]
        val underTest = new NodeGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.currentMempoolContains)
          .expects(transactionId)
          .once()
          .returning(false.pure[F])

        for {
          _ <- assertIO(
            underTest.currentMempoolContains(CurrentMempoolContainsReq(transactionId), new Metadata()),
            CurrentMempoolContainsRes(inMempool = false)
          )
        } yield ()
      }
    }
  }

  test("FetchNodeConfig can be retrieved") {

    withMock {
      val interpreter = mock[NodeRpc[F, Stream[F, *]]]
      val underTest = new NodeGrpc.Server.GrpcServerImpl[F](interpreter)
      val nodeConfig = Seq(NodeConfig(0, 100, 300), NodeConfig(1, 200, 600))

      (() => interpreter.fetchProtocolConfigs())
        .expects()
        .once()
        .returning(Stream.emits(nodeConfig).pure[F])

      for {
        _ <- assertIO(
          underTest.fetchNodeConfig(FetchNodeConfigReq(), new Metadata()).compile.toList,
          List(FetchNodeConfigRes(nodeConfig.head), FetchNodeConfigRes(nodeConfig.tail.head))
        )
      } yield ()
    }

  }

  test("FetchEpochData can be retrieved") {

    withMock {
      val interpreter = mock[NodeRpc[F, Stream[F, *]]]
      val underTest = new NodeGrpc.Server.GrpcServerImpl[F](interpreter)
      val epochData = EpochData.defaultInstance

      (interpreter.fetchEpochData)
        .expects(0L.some)
        .once()
        .returning(epochData.some.pure[F])

      for {
        _ <- assertIO(
          underTest.fetchEpochData(FetchEpochDataReq(epoch = 0L.some), new Metadata()),
          FetchEpochDataRes(epochData.some)
        )
      } yield ()
    }

  }

  test("The block ID at a depth can be retrieved") {
    PropF.forAllF { (depth: Long, header: BlockHeader) =>
      val blockId = header.id
      withMock {
        val interpreter = mock[NodeRpc[F, Stream[F, *]]]
        val underTest = new NodeGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.blockIdAtDepth)
          .expects(depth)
          .once()
          .returning(blockId.some.pure[F])

        for {
          res <- underTest.fetchBlockIdAtDepth(FetchBlockIdAtDepthReq(depth), new Metadata())
          proto = res.blockId.get
          _ = assert(blockId == proto)
        } yield ()
      }
    }
  }

  test("Retrieve list of transactions from mempool") {
    PropF.forAllF { (transactions: Set[IoTransaction]) =>
      val ids = transactions.map(_.id)
      withMock {
        val interpreter = mock[NodeRpc[F, Stream[F, *]]]
        val underTest = new NodeGrpc.Server.GrpcServerImpl[F](interpreter)

        (() => interpreter.currentMempool())
          .expects()
          .once()
          .returning(ids.pure[F])

        for {
          res <- underTest.currentMempool(CurrentMempoolReq(), new Metadata())
          proto = res.transactionIds.toList
          _ = assert(ids.toList == proto)
        } yield ()
      }
    }
  }

  test("Retrieve Canonical Head Id") {
    PropF.forAllF { (header: BlockHeader) =>
      val blockId = header.id
      withMock {
        val interpreter = mock[NodeRpc[F, Stream[F, *]]]
        val underTest = new NodeGrpc.Server.GrpcServerImpl[F](interpreter)

        (() => interpreter.fetchCanonicalHeadId())
          .expects()
          .once()
          .returning(blockId.some.pure[F])

        assertIO(
          underTest.fetchCanonicalHeadId(FetchCanonicalHeadIdReq(), new Metadata()),
          FetchCanonicalHeadIdRes(blockId.some)
        )

      }
    }
  }

  // TODO: synchronizationTraversal has no unit Testing
}
