package xyz.stratalab.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.effect.kernel.{Async, Sync}
import cats.effect.{IO, Resource}
import cats.implicits._
import cats.{Applicative, MonadThrow, Show}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import xyz.stratalab.algebras.Store
import xyz.stratalab.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import xyz.stratalab.consensus.algebras.{BlockHeaderToBodyValidationAlgebra, ChainSelectionAlgebra, LocalChainAlgebra}
import xyz.stratalab.consensus.models.{BlockId, SlotData}
import xyz.stratalab.crypto.signing.Ed25519VRF
import xyz.stratalab.eventtree.ParentChildTree
import xyz.stratalab.ledger.algebras.MempoolAlgebra
import xyz.stratalab.models.ModelGenerators.GenHelper
import xyz.stratalab.models.generators.consensus.ModelGenerators._
import xyz.stratalab.models.p2p._
import xyz.stratalab.networking.KnownHostOps
import xyz.stratalab.networking.blockchain.BlockchainPeerClient
import xyz.stratalab.networking.fsnetwork.NetworkQualityError.{IncorrectPongMessage, NoPongMessage}
import xyz.stratalab.networking.fsnetwork.PeerActorTest.F
import xyz.stratalab.networking.fsnetwork.PeerBlockBodyFetcher.PeerBlockBodyFetcherActor
import xyz.stratalab.networking.fsnetwork.PeerBlockHeaderFetcher.PeerBlockHeaderFetcherActor
import xyz.stratalab.networking.fsnetwork.PeerMempoolTransactionSync.PeerMempoolTransactionSyncActor
import xyz.stratalab.networking.fsnetwork.PeersManager.Message.PingPongMessagePing
import xyz.stratalab.networking.fsnetwork.PeersManager.PeersManagerActor
import xyz.stratalab.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import xyz.stratalab.networking.fsnetwork.TestHelper.{arbitraryHost, arbitraryKnownHost}
import xyz.stratalab.node.models._
import xyz.stratalab.sdk.generators.TransactionGenerator
import xyz.stratalab.sdk.models.TransactionId
import xyz.stratalab.sdk.models.transaction.IoTransaction
import xyz.stratalab.sdk.validation.algebras.TransactionSyntaxVerifier

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

object PeerActorTest {
  type F[A] = IO[A]
}

class PeerActorTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory with TransactionGenerator {

  case class PeerActorMockData(
    peersManager:                    PeersManagerActor[F],
    requestsProxy:                   RequestsProxyActor[F],
    localChain:                      LocalChainAlgebra[F],
    chainSelection:                  ChainSelectionAlgebra[F, BlockId, SlotData],
    slotDataStore:                   Store[F, BlockId, SlotData],
    bodyDataStore:                   Store[F, BlockId, BlockBody],
    transactionStore:                Store[F, TransactionId, IoTransaction],
    headerToBodyValidation:          BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation:     TransactionSyntaxVerifier[F],
    mempool:                         MempoolAlgebra[F],
    client:                          BlockchainPeerClient[F],
    networkAlgebra:                  NetworkAlgebra[F],
    blockHeaderFetcher:              PeerBlockHeaderFetcherActor[F],
    blockBodyFetcher:                PeerBlockBodyFetcherActor[F],
    peerMempoolTransactionSyncActor: PeerMempoolTransactionSyncActor[F],
    ed255Vrf:                        Resource[F, Ed25519VRF],
    blockIdTree:                     ParentChildTree[F, BlockId]
  ) {}

  private def buildDefaultMockData(client: BlockchainPeerClient[F] = createDummyClient()): PeerActorMockData = {
    val (networkAlgebra, peerBlockHeaderFetcherActor, peerBlockBodyFetcherActor, peerMempoolTransactionSyncActor) =
      createDummyNetworkAlgebra()

    val peersManager: PeersManagerActor[F] = mock[PeersManagerActor[F]]
    // ping message request after start network level
    (peersManager.sendNoWait).expects(*).once().returns(Applicative[F].unit)

    buildMockData(
      networkAlgebra,
      peerBlockHeaderFetcherActor,
      peerBlockBodyFetcherActor,
      peerMempoolTransactionSyncActor,
      client,
      peersManager = peersManager
    )
  }
  val defaultEd255Vrf: Resource[F, Ed25519VRF] = Resource.pure(Ed25519VRF.precomputed())

  // scalastyle:off parameter.number
  private def buildMockData(
    networkAlgebra:                  NetworkAlgebra[F],
    peerBlockHeaderFetcherActor:     PeerBlockHeaderFetcherActor[F],
    peerBlockBodyFetcherActor:       PeerBlockBodyFetcherActor[F],
    peerMempoolTransactionSyncActor: PeerMempoolTransactionSyncActor[F],
    client:                          BlockchainPeerClient[F] = createDummyClient(),
    peersManager:                    PeersManagerActor[F] = mock[PeersManagerActor[F]],
    requestsProxy:                   RequestsProxyActor[F] = mock[RequestsProxyActor[F]],
    localChain:                      LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]],
    chainSelection:   ChainSelectionAlgebra[F, BlockId, SlotData] = mock[ChainSelectionAlgebra[F, BlockId, SlotData]],
    slotDataStore:    Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]],
    bodyDataStore:    Store[F, BlockId, BlockBody] = mock[Store[F, BlockId, BlockBody]],
    transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F] = mock[TransactionSyntaxVerifier[F]],
    mempool:                     MempoolAlgebra[F] = mock[MempoolAlgebra[F]],
    ed255Vrf:                    Resource[F, Ed25519VRF] = defaultEd255Vrf,
    blockIdTree:                 ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
  ): PeerActorMockData = {
    (() => localChain.genesis).expects().once().returning(genesis.pure[F])
    PeerActorMockData(
      peersManager,
      requestsProxy,
      localChain,
      chainSelection,
      slotDataStore,
      bodyDataStore,
      transactionStore,
      headerToBodyValidation,
      transactionSyntaxValidation,
      mempool,
      client,
      networkAlgebra,
      peerBlockHeaderFetcherActor,
      peerBlockBodyFetcherActor,
      peerMempoolTransactionSyncActor,
      ed255Vrf,
      blockIdTree
    )
  }
  // scalastyle:on parameter.number

  private def buildPeerActorFromMockedData(peerActorMockData: PeerActorMockData): Resource[F, PeerActor.PeerActor[F]] =
    PeerActor
      .makeActor(
        hostId,
        peerActorMockData.networkAlgebra,
        peerActorMockData.client,
        peerActorMockData.requestsProxy,
        peerActorMockData.peersManager,
        peerActorMockData.localChain,
        peerActorMockData.chainSelection,
        peerActorMockData.slotDataStore,
        peerActorMockData.bodyDataStore,
        peerActorMockData.transactionStore,
        peerActorMockData.headerToBodyValidation,
        peerActorMockData.transactionSyntaxValidation,
        peerActorMockData.mempool,
        commonAncestor[F],
        peerActorMockData.ed255Vrf,
        peerActorMockData.blockIdTree
      )

  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  val hostId: HostId = arbitraryHost.arbitrary.first

  private val genesis = arbitrarySlotData.arbitrary.first

  private def createDummyClient(): BlockchainPeerClient[F] = {
    val client = mock[BlockchainPeerClient[F]]
    (client
      .getRemoteBlockIdAtHeight(_: Long))
      .expects(1L)
      .once()
      .returning(genesis.slotId.blockId.some.pure[F])
    (client.getPongMessage).stubs(*).onCall { (ping: PingMessage) =>
      Option(PongMessage(ping.ping.reverse)).pure[F]
    }
    (client.notifyAboutThisNetworkLevel).stubs(*).returns(Applicative[F].unit)
    ((() => client.closeConnection())).stubs().returns(Applicative[F].unit)

    client
  }

  private def createDummyNetworkAlgebra(): (
    NetworkAlgebra[F],
    PeerBlockHeaderFetcherActor[F],
    PeerBlockBodyFetcherActor[F],
    PeerMempoolTransactionSyncActor[F]
  ) = {
    val networkAlgebra = mock[NetworkAlgebra[F]]
    val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
    (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
    (networkAlgebra.makePeerHeaderFetcher)
      .expects(*, *, *, *, *, *, *, *, *, *, *)
      .returns(
        // simulate real header fetcher behaviour on finalizing
        Resource
          .pure(blockHeaderFetcher)
          .onFinalize(Sync[F].defer(blockHeaderFetcher.sendNoWait(PeerBlockHeaderFetcher.Message.StopActor)))
      )

    val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
    (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
    (networkAlgebra.makePeerBodyFetcher)
      .expects(*, *, *, *, *, *)
      .returns(
        // simulate real body fetcher behaviour on finalizing
        Resource
          .pure(blockBodyFetcher)
          .onFinalize(Sync[F].defer(blockBodyFetcher.sendNoWait(PeerBlockBodyFetcher.Message.StopActor)))
      )

    val mempoolTransactionSync = mock[PeerMempoolTransactionSyncActor[F]]
    (() => mempoolTransactionSync.id).expects().anyNumberOfTimes().returns(3)
    (networkAlgebra.makeMempoolSyncFetcher)
      .expects(*, *, *, *, *, *, *)
      .returns(
        // simulate real body fetcher behaviour on finalizing
        Resource
          .pure(mempoolTransactionSync)
          .onFinalize(Sync[F].defer(mempoolTransactionSync.sendNoWait(PeerMempoolTransactionSync.Message.StopActor)))
      )

    (blockHeaderFetcher.sendNoWait).expects(PeerBlockHeaderFetcher.Message.StartActor).returns(Applicative[F].unit)
    (blockHeaderFetcher.sendNoWait).expects(PeerBlockHeaderFetcher.Message.StopActor).returns(Applicative[F].unit)

    (blockBodyFetcher.sendNoWait).expects(PeerBlockBodyFetcher.Message.StartActor).returns(Applicative[F].unit)
    (blockBodyFetcher.sendNoWait).expects(PeerBlockBodyFetcher.Message.StopActor).returns(Applicative[F].unit)

    (mempoolTransactionSync.sendNoWait)
      .expects(PeerMempoolTransactionSync.Message.StartActor)
      .returns(Applicative[F].unit)
    (mempoolTransactionSync.sendNoWait)
      .expects(PeerMempoolTransactionSync.Message.StopActor)
      .returns(Applicative[F].unit)

    (networkAlgebra, blockHeaderFetcher, blockBodyFetcher, mempoolTransactionSync)
  }

  test("Setting application level to true shall starts sub-actors") {
    withMock {
      val mockData = buildDefaultMockData()
      buildPeerActorFromMockedData(mockData)
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
          } yield ()
        }
    }
  }

  test("Block header download shall be forwarded to header fetcher") {
    withMock {
      val mockData = buildDefaultMockData()
      val blockHeader = arbitraryHeader.arbitrary.first.embedId
      (mockData.blockHeaderFetcher.sendNoWait)
        .expects(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(NonEmptyChain.one(blockHeader.id)))
        .returns(Applicative[F].unit)

      buildPeerActorFromMockedData(mockData)
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
            _ <- actor.send(PeerActor.Message.DownloadBlockHeaders(NonEmptyChain.one(blockHeader.id)))
          } yield ()
        }
    }
  }

  test("Block body download shall be forwarded to body fetcher") {
    withMock {
      val mockData = buildDefaultMockData()
      val blockHeader = arbitraryHeader.arbitrary.first.embedId
      (mockData.blockBodyFetcher.sendNoWait)
        .expects(PeerBlockBodyFetcher.Message.DownloadBlocks(NonEmptyChain.one(blockHeader)))
        .returns(Applicative[F].unit)

      buildPeerActorFromMockedData(mockData)
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
            _ <- actor.send(PeerActor.Message.DownloadBlockBodies(NonEmptyChain.one(blockHeader)))
          } yield ()
        }
    }
  }

  test("Reputation update tick shall be forwarded") {
    withMock {
      val mockData = buildDefaultMockData()
      (mockData.peerMempoolTransactionSyncActor.sendNoWait)
        .expects(PeerMempoolTransactionSync.Message.CollectTransactionsRep)
        .returns(Applicative[F].unit)

      buildPeerActorFromMockedData(mockData)
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
            _ <- actor.send(PeerActor.Message.ReputationUpdateTick)
          } yield ()
        }
    }
  }

  test("Ping shall be started and result is sent to reputation aggregator") {
    withMock {
      val pingDelay = FiniteDuration(10, MILLISECONDS)

      val client = mock[BlockchainPeerClient[F]]
      (client.notifyAboutThisNetworkLevel).expects(true).returns(Applicative[F].unit)
      (client.getPongMessage).expects(*).atLeastOnce().onCall { (ping: PingMessage) =>
        Async[F].delayBy(Option(PongMessage(ping.ping.reverse)).pure[F], pingDelay)
      }
      (client
        .getRemoteBlockIdAtHeight(_: Long))
        .expects(1L)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      (client.notifyAboutThisNetworkLevel).expects(false).returns(Applicative[F].unit)
      ((() => client.closeConnection())).expects().returns(Applicative[F].unit)

      val (networkAlgebra, blockHeaderFetcher, blockBodyFetcher, mempoolSync) = createDummyNetworkAlgebra()
      val mockedData = buildMockData(networkAlgebra, blockHeaderFetcher, blockBodyFetcher, mempoolSync, client)

      (mockedData.peersManager.sendNoWait).expects(*).atLeastOnce().onCall { (message: PeersManager.Message) =>
        message match {
          case PingPongMessagePing(`hostId`, Right(t)) =>
            assert(t >= pingDelay.toMillis)
            // scala fmt joing these lines
            ().pure[F]
          case _ => throw new IllegalStateException()
        }
      }

      buildPeerActorFromMockedData(mockedData)
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
            _ <- Async[F].andWait(actor.send(PeerActor.Message.GetNetworkQuality), pingDelay * 5)
          } yield ()
        }
    }

  }

  test("Ping shall be started: one success and two errors") {
    withMock {
      val pingDelay: FiniteDuration = FiniteDuration(10, MILLISECONDS)

      val client = mock[BlockchainPeerClient[F]]
      (client.notifyAboutThisNetworkLevel).expects(true).returns(Applicative[F].unit)
      (client.getPongMessage).expects(*).once().onCall { (ping: PingMessage) =>
        Async[F].delayBy(Option(PongMessage(ping.ping.reverse)).pure[F], pingDelay)
      }
      (client.getPongMessage).expects(*).once().onCall { (_: PingMessage) =>
        Option.empty[PongMessage].pure[F]
      }
      (client.getPongMessage).expects(*).atLeastOnce().onCall { (ping: PingMessage) =>
        Option(PongMessage(ping.ping)).pure[F]
      }
      (client
        .getRemoteBlockIdAtHeight(_: Long))
        .expects(1L)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      (client.notifyAboutThisNetworkLevel).expects(false).returns(Applicative[F].unit)
      ((() => client.closeConnection())).expects().returns(Applicative[F].unit)

      val (networkAlgebra, blockHeaderFetcher, blockBodyFetcher, mempoolSync) = createDummyNetworkAlgebra()
      val mockedData = buildMockData(networkAlgebra, blockHeaderFetcher, blockBodyFetcher, mempoolSync, client)

      (mockedData.peersManager.sendNoWait).expects(*).once().onCall { (message: PeersManager.Message) =>
        message match {
          case PingPongMessagePing(`hostId`, Right(t)) =>
            assert(t >= pingDelay.toMillis)
            // scala fmt
            ().pure[F]
          case _ => throw new IllegalStateException()
        }
      }
      (mockedData.peersManager.sendNoWait).expects(*).once().onCall { (message: PeersManager.Message) =>
        message match {
          case PingPongMessagePing(`hostId`, Left(NoPongMessage)) => ().pure[F]
          case _                                                  => throw new IllegalStateException()
        }
      }

      (mockedData.peersManager.sendNoWait).expects(*).atLeastOnce().onCall { (message: PeersManager.Message) =>
        message match {
          case PingPongMessagePing(`hostId`, Left(IncorrectPongMessage)) => ().pure[F]
          case _                                                         => throw new IllegalStateException()
        }
      }

      buildPeerActorFromMockedData(mockedData)
        .use { actor =>
          actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true)) >>
          actor.send(PeerActor.Message.GetNetworkQuality) >>
          actor.send(PeerActor.Message.GetNetworkQuality) >>
          actor.send(PeerActor.Message.GetNetworkQuality)
        }
    }

  }

  test("Ping error shall be processed correctly") {
    withMock {

      val client = mock[BlockchainPeerClient[F]]
      (client.notifyAboutThisNetworkLevel).expects(true).returns(Applicative[F].unit)
      (client
        .getRemoteBlockIdAtHeight(_: Long))
        .expects(1L)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      (client.getPongMessage).expects(*).twice().onCall((_: PingMessage) => throw new RuntimeException())
      (client.notifyAboutThisNetworkLevel).expects(false).returns(Applicative[F].unit)
      ((() => client.closeConnection())).expects().returns(Applicative[F].unit)

      val (networkAlgebra, blockHeaderFetcher, blockBodyFetcher, mempoolSync) = createDummyNetworkAlgebra()
      val mockedData = buildMockData(networkAlgebra, blockHeaderFetcher, blockBodyFetcher, mempoolSync, client)

      (mockedData.peersManager.sendNoWait)
        .expects(PeersManager.Message.NonCriticalErrorForHost(hostId))
        .twice()
        .returns(Applicative[F].unit)

      buildPeerActorFromMockedData(mockedData)
        .use { actor =>
          actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true)) >>
          actor.send(PeerActor.Message.GetNetworkQuality)
        }
    }

  }

  test("Common ancestor shall be tracked") {
    withMock {
      val commonAncestorSlotData = arbitrarySlotData.arbitrary.first
      val commonAncestor = commonAncestorSlotData.slotId.blockId

      val mockedData = buildDefaultMockData()
      (mockedData.client
        .findCommonAncestor(_: Long => F[BlockId], _: () => F[Long])(_: Sync[F], _: Logger[F]))
        .expects(*, *, *, *)
        .once()
        .returns(commonAncestor.pure[F])

      (mockedData.slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(commonAncestor, *, *)
        .returns(commonAncestorSlotData.pure[F])

      buildPeerActorFromMockedData(mockedData)
        .use { actor =>
          actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true)) >>
          actor.send(PeerActor.Message.PrintCommonAncestor)
        }
    }

  }

  test("Common ancestor error shall be processed") {
    withMock {

      val commonAncestorSlotData = arbitrarySlotData.arbitrary.first
      val commonAncestor = commonAncestorSlotData.slotId.blockId

      val mockedData = buildDefaultMockData()

      (mockedData.client
        .findCommonAncestor(_: Long => F[BlockId], _: () => F[Long])(_: Sync[F], _: Logger[F]))
        .expects(*, *, *, *)
        .once()
        .returns(Sync[F].delay(throw new RuntimeException()))

      (mockedData.slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(commonAncestor, *, *)
        .never()
        .returns(commonAncestorSlotData.pure[F])

      (mockedData.peersManager.sendNoWait)
        .expects(PeersManager.Message.NonCriticalErrorForHost(hostId))
        .returns(Applicative[F].unit)

      buildPeerActorFromMockedData(mockedData)
        .use { actor =>
          actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true)) >>
          actor.send(PeerActor.Message.PrintCommonAncestor)
        }
    }

  }

  test("Request to get current tip shall be forwarded to block header fetcher") {
    withMock {
      val mockedData = buildDefaultMockData()

      (mockedData.blockHeaderFetcher.sendNoWait)
        .expects(PeerBlockHeaderFetcher.Message.GetCurrentTip)
        .returns(Applicative[F].unit)

      buildPeerActorFromMockedData(mockedData)
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
            _ <- actor.send(PeerActor.Message.GetCurrentTip)
          } yield ()
        }
    }
  }

  test("Request to get peer neighbours shall be processed if non-empty known hosts received") {
    withMock {
      val mockedData = buildDefaultMockData()

      val host1 = arbitraryKnownHost.arbitrary.first
      val host2 = arbitraryKnownHost.arbitrary.first
      val hosts = Seq(host1, host2)
      (mockedData.client.getRemoteKnownHosts)
        .expects(CurrentKnownHostsReq(2))
        .returns(Option(CurrentKnownHostsRes(hosts)).pure[F])

      (mockedData.peersManager.sendNoWait)
        .expects(
          PeersManager.Message.AddKnownNeighbors(hostId, NonEmptyChain.fromSeq(hosts.map(_.asRemotePeer)).get)
        )
        .returns(Applicative[F].unit)

      buildPeerActorFromMockedData(mockedData)
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
            _ <- actor.send(PeerActor.Message.GetHotPeersFromPeer(2))
          } yield ()
        }
    }
  }

  test("Request to get peer neighbours shall not be processed if empty known hosts received") {
    withMock {
      val mockedData = buildDefaultMockData()

      (mockedData.client.getRemoteKnownHosts)
        .expects(CurrentKnownHostsReq(2))
        .returns(Option(CurrentKnownHostsRes()).pure[F])

      buildPeerActorFromMockedData(mockedData)
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
            _ <- actor.send(PeerActor.Message.GetHotPeersFromPeer(2))
          } yield ()
        }
    }
  }

  test("Error of request to get peer neighbours shall be processed") {
    withMock {
      val mockedData = buildDefaultMockData()

      (mockedData.client.getRemoteKnownHosts)
        .expects(CurrentKnownHostsReq(2))
        .onCall((_: CurrentKnownHostsReq) => throw new RuntimeException())

      (mockedData.peersManager.sendNoWait)
        .expects(PeersManager.Message.NonCriticalErrorForHost(hostId))
        .returns(Applicative[F].unit)

      buildPeerActorFromMockedData(mockedData)
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
            _ <- actor.send(PeerActor.Message.GetHotPeersFromPeer(2))
          } yield ()
        }
    }
  }

  test("Close connection shall be passed to the client") {
    withMock {

      val client = mock[BlockchainPeerClient[F]]
      (client
        .getRemoteBlockIdAtHeight(_: Long))
        .expects(1L)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      (client.getPongMessage).stubs(*).onCall { (ping: PingMessage) =>
        Option(PongMessage(ping.ping.reverse)).pure[F]
      }
      (client.notifyAboutThisNetworkLevel).stubs(*).returns(Applicative[F].unit)
      ((() => client.closeConnection())).expects().atLeastOnce().returns(Applicative[F].unit)

      val mockedData = buildDefaultMockData(client)

      (mockedData.blockBodyFetcher.sendNoWait)
        .expects(PeerBlockBodyFetcher.Message.StopActor)
        .returning(Applicative[F].unit)
      (mockedData.blockHeaderFetcher.sendNoWait)
        .expects(PeerBlockHeaderFetcher.Message.StopActor)
        .returning(Applicative[F].unit)
      (mockedData.peerMempoolTransactionSyncActor.sendNoWait)
        .expects(PeerMempoolTransactionSync.Message.StopActor)
        .returning(Applicative[F].unit)

      buildPeerActorFromMockedData(mockedData)
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
            _ <- actor.send(PeerActor.Message.CloseConnectionForActor)
          } yield ()
        }
    }
  }

  test("Mismatched genesis block should result non critical error message to reputation aggregator") {
    withMock {
      val localGenesis = arbitrarySlotData.arbitrary.first
      val remoteGenesisId = arbitraryBlockId.arbitrary.first
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(localGenesis.pure[F])
      val chainSelection = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val bodyDataStore = mock[Store[F, BlockId, BlockBody]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val client = mock[BlockchainPeerClient[F]]
      (client.notifyAboutThisNetworkLevel).stubs(*).returns(Applicative[F].unit)
      (client
        .getRemoteBlockIdAtHeight(_: Long))
        .expects(1L)
        .once()
        .returning(remoteGenesisId.some.pure[F])
      ((() => client.closeConnection())).stubs().returns(Applicative[F].unit)
      (peersManager.sendNoWait)
        .expects(PeersManager.Message.NonCriticalErrorForHost(hostId))
        .once()
        .returns(Applicative[F].unit)

      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      (networkAlgebra.makePeerHeaderFetcher)
        .expects(*, *, *, *, *, *, *, *, *, *, *)
        .returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (networkAlgebra.makePeerBodyFetcher).expects(*, *, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      val mempoolSync = mock[PeerMempoolTransactionSyncActor[F]]
      (() => mempoolSync.id).expects().anyNumberOfTimes().returns(3)
      (networkAlgebra.makeMempoolSyncFetcher).expects(*, *, *, *, *, *, *).returns(Resource.pure(mempoolSync))

      val ed255Vrf: Resource[F, Ed25519VRF] = defaultEd255Vrf
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          peersManager,
          localChain,
          chainSelection,
          slotDataStore,
          bodyDataStore,
          transactionStore,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool,
          commonAncestor[F],
          ed255Vrf,
          blockIdTree
        )
        .use(_ => Applicative[F].unit)
    }
  }

  test("Finalizer shall call appropriate calls to client") {
    withMock {
      arbitrarySlotData.arbitrary.first
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val chainSelection = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val bodyDataStore = mock[Store[F, BlockId, BlockBody]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = mock[BlockchainPeerClient[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val (networkAlgebra, _, _, _) = createDummyNetworkAlgebra()
      (client.notifyAboutThisNetworkLevel).expects(true).returns(Applicative[F].unit)
      (client
        .getRemoteBlockIdAtHeight(_: Long))
        .expects(1L)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      (client.getPongMessage).stubs(*).onCall { (ping: PingMessage) =>
        Option(PongMessage(ping.ping.reverse)).pure[F]
      }
      (peersManager.sendNoWait).stubs(*).returns(Applicative[F].unit)
      ((() => client.closeConnection())).stubs().returns(Applicative[F].unit)
      (client.notifyAboutThisNetworkLevel).expects(false).returns(Applicative[F].unit)

      val ed255Vrf: Resource[F, Ed25519VRF] = defaultEd255Vrf
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          peersManager,
          localChain,
          chainSelection,
          slotDataStore,
          bodyDataStore,
          transactionStore,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool,
          commonAncestor[F],
          ed255Vrf,
          blockIdTree
        )
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
          } yield ()
        }
    }
  }
}
