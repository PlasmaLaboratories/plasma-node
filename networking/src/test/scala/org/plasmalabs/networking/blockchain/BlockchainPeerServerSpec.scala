package org.plasmalabs.networking.blockchain

import cats.data.NonEmptyChain
import cats.effect.{IO, Resource}
import cats.implicits.*
import cats.{MonadThrow, Show}
import fs2.*
import fs2.concurrent.Topic
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.algebras.Store
import org.plasmalabs.blockchain.{BlockchainCore, DataStores}
import org.plasmalabs.consensus.Consensus
import org.plasmalabs.consensus.algebras.LocalChainAlgebra
import org.plasmalabs.consensus.models.*
import org.plasmalabs.ledger.Ledger
import org.plasmalabs.ledger.algebras.{MempoolAlgebra, TransactionRewardCalculatorAlgebra}
import org.plasmalabs.ledger.models.{MempoolGraph, RewardQuantities}
import org.plasmalabs.models.ModelGenerators.*
import org.plasmalabs.models.generators.consensus.ModelGenerators.*
import org.plasmalabs.models.generators.node.ModelGenerators.*
import org.plasmalabs.networking.NetworkGen.*
import org.plasmalabs.networking.fsnetwork.RemotePeer
import org.plasmalabs.networking.fsnetwork.TestHelper.*
import org.plasmalabs.networking.p2p.PeerConnectionChanges.RemotePeerApplicationLevel
import org.plasmalabs.networking.p2p.{ConnectedPeer, PeerConnectionChange}
import org.plasmalabs.node.models.{BlockBody, CurrentKnownHostsReq, CurrentKnownHostsRes, KnownHost}
import org.plasmalabs.sdk.models.TransactionId
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.validation.algebras.TransactionCostCalculator
import org.scalacheck.Gen
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import scala.concurrent.duration.*

class BlockchainPeerServerSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  private val dummyRewardCalc: TransactionRewardCalculatorAlgebra = (_: IoTransaction) => RewardQuantities()
  private val dummyCostCalc: TransactionCostCalculator = (tx: IoTransaction) => tx.inputs.size

  override val munitIOTimeout: FiniteDuration = 5.seconds

  test("serve slot data") {
    PropF.forAllF { (slotData: SlotData) =>
      withMock {
        val slotDataStore = mock[Store[F, BlockId, SlotData]]
        (slotDataStore.get(_: BlockId)).expects(slotData.slotId.blockId).once().returning(slotData.some.pure[F])
        makeServer(slotDataStore = slotDataStore)
          .use(underTest => underTest.getLocalSlotData(slotData.slotId.blockId).assertEquals(slotData.some))
      }
    }
  }

  val chainLen = 100L

  test("serve slot data chain") {
    PropF.forAllF(Gen.choose(1L, chainLen), Gen.choose(1L, chainLen), Gen.choose(1L, chainLen + 1)) {
      (firstVal: Long, secondVal: Long, chunkSize: Long) =>
        withMock {
          val from = Math.min(firstVal, secondVal)
          val to = Math.max(firstVal, secondVal)

          val preGenesis = arbitrarySlotData.arbitrary.first.copy(height = 0)
          val slotDataChain: NonEmptyChain[SlotData] =
            arbitraryLinkedSlotDataChainFor(Gen.choose(chainLen, chainLen), preGenesis.some).arbitrary.first
          val slotDataMap = slotDataChain.map(sd => (sd.slotId.blockId, sd)).toList.toMap
          val heightToId = slotDataChain.map(sd => (sd.height, sd.slotId.blockId)).toList.toMap
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          (slotDataStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
            slotDataMap.contains(id).pure[F]
          }
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              slotDataMap(id).pure[F]
            }

          makeServer(slotDataStore = slotDataStore, slotDataParentDepth = chunkSize.toInt).use { underTest =>
            for {
              resOpt <- underTest.requestSlotDataAndParents(heightToId(from), heightToId(to))
              res = resOpt.get
              expectedSize = Math.min((to - from) + 1, chunkSize + 1)
              _        <- assert(res.size == expectedSize).pure[F]
              _        <- assert(res.head.slotId.blockId == heightToId(to - expectedSize + 1)).pure[F]
              _        <- assert(res.last.slotId.blockId == heightToId(to)).pure[F]
              _        <- assert(res.forall(sd => sd.height >= from && sd.height <= to)).pure[F]
              _        <- assert(res.forall(sd => sd.slotId.blockId == heightToId(sd.height))).pure[F]
              _        <- assert(res.map(sd => sd.height).toSet.size == expectedSize).pure[F]
              _        <- assert(res.map(sd => sd.height).sorted == res.map(sd => sd.height)).pure[F]
              resToGen <- underTest.requestSlotDataAndParents(slotDataChain.head.parentSlotId.blockId, heightToId(to))
              _ <- assert(if (to > 1) resToGen.get.size == Math.min(to, chunkSize + 1) else resToGen.isEmpty).pure[F]
              empty <- underTest.requestSlotDataAndParents(heightToId(to), slotDataChain.head.parentSlotId.blockId)
              _     <- assert(empty.isEmpty).pure[F]
            } yield ()
          }.void
        }
    }
  }

  test("serve headers") {
    PropF.forAllF { (header: BlockHeader, id: BlockId) =>
      withMock {
        val headerStore = mock[Store[F, BlockId, BlockHeader]]
        (headerStore.get(_: BlockId)).expects(id).once().returning(header.some.pure[F])
        makeServer(headerStore = headerStore)
          .use(underTest => underTest.getLocalHeader(id).assertEquals(header.some))
      }
    }
  }

  test("serve bodies") {
    PropF.forAllF { (body: BlockBody, id: BlockId) =>
      withMock {
        val bodyStore = mock[Store[F, BlockId, BlockBody]]
        (bodyStore.get(_: BlockId)).expects(id).once().returning(body.some.pure[F])
        makeServer(bodyStore = bodyStore)
          .use(underTest => underTest.getLocalBody(id).assertEquals(body.some))
      }
    }
  }

  test("serve transactions") {
    PropF.forAllF { (transaction: IoTransaction, id: TransactionId) =>
      withMock {
        val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
        (transactionStore.get(_: TransactionId)).expects(id).once().returning(transaction.some.pure[F])
        makeServer(transactionStore = transactionStore)
          .use(underTest => underTest.getLocalTransaction(id).assertEquals(transaction.some))
      }
    }
  }

  test("serve this peer address") {
    PropF.forAllF { (asServer: KnownHost) =>
      withMock {
        val f = mockFunction[Option[KnownHost]]
        f.expects().once().returning(Option(asServer))
        makeServer(asServer = f)
          .use(underTest => underTest.peerAsServer.assertEquals(asServer.some))
      }
    }
  }

  test("serve hot peers") {
    PropF.forAllF { (hotPeers: Set[RemotePeer]) =>
      withMock {
        val f = mockFunction[F[Set[RemotePeer]]]
        f.expects().once().returning(hotPeers.pure[F])
        val expected =
          CurrentKnownHostsRes(hotPeers.toSeq.map(rp => KnownHost(rp.peerId.id, rp.address.host, rp.address.port)))
        makeServer(currentHotPeers = f)
          .use(underTest => underTest.getKnownHosts(CurrentKnownHostsReq(hotPeers.size)).assertEquals(expected.some))
      }
    }
  }

  test("serve hot peers, we have more than requested") {
    withMock {
      val host1 = arbitraryRemotePeer.arbitrary.first
      val host2 = arbitraryRemotePeer.arbitrary.first

      val allPeers: Set[RemotePeer] = Set(host1, host2)

      val f = mockFunction[F[Set[RemotePeer]]]
      f.expects().once().returning(allPeers.pure[F])
      val expected = CurrentKnownHostsRes(Seq(KnownHost(host1.peerId.id, host1.address.host, host1.address.port)))
      makeServer(currentHotPeers = f)
        .use(underTest => underTest.getKnownHosts(CurrentKnownHostsReq(1)).assertEquals(expected.some))
    }

  }

  test("serve block ID at height") {
    PropF.forAllF { (height: Long, resultId: BlockId) =>
      withMock {
        val localChain = mock[LocalChainAlgebra[F]]
        (() => localChain.adoptions)
          .expects()
          .anyNumberOfTimes()
          .returning(Stream.never[F].pure[F])
        (localChain.blockIdAtHeight(_: Long)).expects(height).once().returning(resultId.some.pure[F])
        makeServer(localChain = localChain)
          .use(underTest => underTest.getLocalBlockAtHeight(height).assertEquals(resultId.some))
      }
    }
  }

  test("serve local block adoptions") {
    PropF.forAllF { (head: SlotData, adoptionA: BlockId, adoptionB: BlockId, adoptionC: BlockId) =>
      withMock {
        val localChain = mock[LocalChainAlgebra[F]]
        (() => localChain.head).expects().once().returning(head.pure[F])
        (() => localChain.adoptions).expects().once().returning(Stream(adoptionA, adoptionB, adoptionC).pure[F])
        for {
          result <- makeServer(localChain = localChain)
            .use(server => Stream.force(server.localBlockAdoptions).compile.toList)
          _ <- IO(result).assertEquals(List(head.slotId.blockId, adoptionA, adoptionB, adoptionC))
        } yield ()
      }
    }
  }

  test("serve changed connection") {
    PropF.forAllF { (enable1: Boolean, enable2: Boolean) =>
      withMock {
        val underTest =
          for {
            topic        <- Resource.liftK(Topic[F, PeerConnectionChange])
            peer         <- Resource.pure(arbitraryConnectedPeer.arbitrary.first)
            changeStream <- topic.subscribeAwaitUnbounded
            server       <- makeServer(connectionStatusF = topic.pure[F], peer = peer)
          } yield (server, peer, changeStream, topic)

        underTest.use { case (server, peer, stream, topic) =>
          for {
            _   <- server.notifyApplicationLevel(enable1)
            _   <- server.notifyApplicationLevel(enable2)
            _   <- topic.close
            res <- stream.compile.toList
            _ = assert(res.head match {
              case RemotePeerApplicationLevel(`peer`, `enable1`) => true
              case _                                             => false
            })
            _ = assert(res.last match {
              case RemotePeerApplicationLevel(`peer`, `enable2`) => true
              case _                                             => false
            })
          } yield ()
        }
      }
    }
  }

  test("serve local transaction notifications") {
    PropF.forAllF {
      (
        head:       SlotData,
        mempoolTxA: TransactionId,
        mempoolTxB: TransactionId,
        mempoolTxC: TransactionId,
        adoptionA:  TransactionId,
        adoptionB:  TransactionId,
        adoptionC:  TransactionId
      ) =>
        withMock {
          val currentMempool = MempoolGraph.fromTxs(
            Map(
              mempoolTxA -> IoTransaction.defaultInstance,
              mempoolTxB -> IoTransaction.defaultInstance,
              mempoolTxC -> IoTransaction.defaultInstance
            ),
            Map.empty,
            Map.empty,
            dummyRewardCalc,
            dummyCostCalc
          )
          val mempool = mock[MempoolAlgebra[F]]
          (mempool.read).expects(head.slotId.blockId).once().returning(currentMempool.pure[F])
          val localChain = mock[LocalChainAlgebra[F]]
          (() => localChain.head).expects().once().returning(head.pure[F])
          (() => localChain.adoptions)
            .expects()
            .anyNumberOfTimes()
            .returning(Stream.never[F].pure[F])
          for {
            topic <- Topic[F, TransactionId]
            publisher = Stream(adoptionA, adoptionB, adoptionC).through(topic.publish)
            _ = (() => mempool.adoptions).expects().once().returning(topic)
            result <- makeServer(localChain = localChain, mempool = mempool)
              .use(server => Stream.force(server.localTransactionNotifications).concurrently(publisher).compile.toList)
            _ <- IO(result).assertEquals(List(mempoolTxA, mempoolTxB, mempoolTxC, adoptionA, adoptionB, adoptionC))
          } yield ()
        }
    }
  }

  private def makeServer(
    slotDataStore:     Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]],
    headerStore:       Store[F, BlockId, BlockHeader] = mock[Store[F, BlockId, BlockHeader]],
    bodyStore:         Store[F, BlockId, BlockBody] = mock[Store[F, BlockId, BlockBody]],
    transactionStore:  Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]],
    asServer:          () => Option[KnownHost] = () => ???,
    connectionStatusF: F[Topic[F, PeerConnectionChange]] = Topic[F, PeerConnectionChange],
    currentHotPeers:   () => F[Set[RemotePeer]] = () => Set.empty[RemotePeer].pure[F],
    localChain: LocalChainAlgebra[F] = {
      val c = mock[LocalChainAlgebra[F]]
      (() => c.adoptions)
        .expects()
        .anyNumberOfTimes()
        .returning(Stream.never[F].pure[F])
      c
    },
    mempool: MempoolAlgebra[F] = {
      val c = mock[MempoolAlgebra[F]]
      val t = mock[Topic[F, TransactionId]]
      (() => t.subscribeAwaitUnbounded)
        .expects()
        .anyNumberOfTimes()
        .returning(Stream.never[F].pure[F].toResource)
      (() => c.adoptions)
        .expects()
        .anyNumberOfTimes()
        .returning(t)
      c
    },
    peer:                ConnectedPeer = arbitraryConnectedPeer.arbitrary.first,
    slotDataParentDepth: Int = 0
  ) =
    Resource
      .eval(connectionStatusF)
      .flatMap { connectionStatus =>
        val dataStores = mock[DataStores[F]]
        (() => dataStores.slotData).expects().anyNumberOfTimes().returning(slotDataStore)
        (() => dataStores.headers).expects().anyNumberOfTimes().returning(headerStore)
        (() => dataStores.bodies).expects().anyNumberOfTimes().returning(bodyStore)
        (() => dataStores.transactions).expects().anyNumberOfTimes().returning(transactionStore)

        val consensus = mock[Consensus[F]]
        (() => consensus.localChain).expects().anyNumberOfTimes().returning(localChain)

        val ledger = mock[Ledger[F]]
        (() => ledger.mempool).expects().anyNumberOfTimes().returning(mempool)

        val blockchain = mock[BlockchainCore[F]]
        (() => blockchain.dataStores).expects().anyNumberOfTimes().returning(dataStores)
        (() => blockchain.consensus).expects().anyNumberOfTimes().returning(consensus)
        (() => blockchain.ledger).expects().anyNumberOfTimes().returning(ledger)

        BlockchainPeerServer.make(blockchain, asServer, currentHotPeers, connectionStatus, slotDataParentDepth)(peer)
      }
}
