package org.plasmalabs.networking.fsnetwork

import cats.data.{NonEmptyChain, OptionT}
import cats.effect.kernel.Sync
import cats.effect.{Async, IO, Resource}
import cats.implicits.*
import cats.{Applicative, MonadThrow, Show}
import fs2.Stream
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.algebras.{ClockAlgebra, Store}
import org.plasmalabs.codecs.bytes.tetra.instances.*
import org.plasmalabs.consensus.algebras.{ChainSelectionAlgebra, LocalChainAlgebra}
import org.plasmalabs.consensus.models.{BlockHeader, BlockId, SlotData}
import org.plasmalabs.crypto.signing.Ed25519VRF
import org.plasmalabs.eventtree.ParentChildTree
import org.plasmalabs.models.ModelGenerators.GenHelper
import org.plasmalabs.models.generators.consensus.ModelGenerators.*
import org.plasmalabs.models.p2p.*
import org.plasmalabs.networking.blockchain.BlockchainPeerClient
import org.plasmalabs.networking.fsnetwork.BlockDownloadError.BlockHeaderDownloadError
import org.plasmalabs.networking.fsnetwork.BlockDownloadError.BlockHeaderDownloadError.*
import org.plasmalabs.networking.fsnetwork.PeerBlockHeaderFetcherTest.F
import org.plasmalabs.networking.fsnetwork.PeersManager.PeersManagerActor
import org.plasmalabs.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import org.plasmalabs.networking.fsnetwork.TestHelper.*
import org.plasmalabs.node.models.BlockBody
import org.scalacheck.Gen
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.mutable
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

object PeerBlockHeaderFetcherTest {
  type F[A] = IO[A]
}

class PeerBlockHeaderFetcherTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  val hostId: HostId = arbitraryHost.arbitrary.first
  val maxChainSize = 99

  val defaultEd255Vrf: Resource[F, Ed25519VRF] = Resource.pure(Ed25519VRF.precomputed())

  val defaultChainSelectionAlgebra: ChainSelectionAlgebra[F, BlockId, SlotData] =
    new ChainSelectionAlgebra[F, BlockId, SlotData] {

      override def compare(
        x:        SlotData,
        y:        SlotData,
        yFetcher: BlockId => F[SlotData],
        xFetcher: BlockId => F[SlotData]
      ): F[Int] =
        x.height.compare(y.height).pure[F]

      override def enoughHeightToCompare(currentHeight: Long, commonHeight: Long, proposedHeight: Long): F[Long] =
        proposedHeight.pure[F]
    }

  case class PeerBlockHeaderMockData(
    hostId:          HostId,
    client:          BlockchainPeerClient[F],
    requestsProxy:   RequestsProxyActor[F],
    peersManager:    PeersManagerActor[F],
    localChain:      LocalChainAlgebra[F],
    chainSelection:  ChainSelectionAlgebra[F, BlockId, SlotData],
    slotDataStore:   Store[F, BlockId, SlotData],
    bodyStore:       Store[F, BlockId, BlockBody],
    clock:           ClockAlgebra[F],
    commonAncestorF: CommonAncestorF[F],
    ed25519VRF:      Resource[F, Ed25519VRF],
    blockIdTree:     ParentChildTree[F, BlockId]
  )

  private def buildDefaultMockData(
    hostId:          HostId = hostId,
    client:          BlockchainPeerClient[F] = mock[BlockchainPeerClient[F]],
    requestsProxy:   RequestsProxyActor[F] = mock[RequestsProxyActor[F]],
    peersManager:    PeersManagerActor[F] = mock[PeersManagerActor[F]],
    localChain:      LocalChainAlgebra[F] = buildLocalChainAlgebra(),
    chainSelection:  ChainSelectionAlgebra[F, BlockId, SlotData] = defaultChainSelectionAlgebra,
    slotDataStore:   Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]],
    bodyStore:       Store[F, BlockId, BlockBody] = defaultBodyStorage,
    clock:           ClockAlgebra[F] = mock[ClockAlgebra[F]],
    commonAncestorF: CommonAncestorF[F] = mock[(BlockchainPeerClient[F], LocalChainAlgebra[F]) => F[BlockId]],
    ed25519VRF:      Resource[F, Ed25519VRF] = defaultEd255Vrf,
    blockIdTree:     ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
  ) =
    PeerBlockHeaderMockData(
      hostId,
      client,
      requestsProxy,
      peersManager,
      localChain,
      chainSelection,
      slotDataStore,
      bodyStore,
      clock,
      commonAncestorF,
      ed25519VRF,
      blockIdTree
    )

  private def buildActorFromMockData(mockData: PeerBlockHeaderMockData) =
    PeerBlockHeaderFetcher
      .makeActor(
        mockData.hostId,
        mockData.client,
        mockData.requestsProxy,
        mockData.peersManager,
        mockData.localChain,
        mockData.chainSelection,
        mockData.slotDataStore,
        mockData.bodyStore,
        mockData.clock,
        mockData.commonAncestorF,
        mockData.ed25519VRF,
        mockData.blockIdTree
      )

  def defaultBodyStorage: Store[F, BlockId, BlockBody] = mock[Store[F, BlockId, BlockBody]]

  def buildLocalChainAlgebra(): LocalChainAlgebra[F] = {
    val localChainAlgebra = mock[LocalChainAlgebra[F]]
    localChainAlgebra
  }

  test("Block header shall be downloaded by request") {
    withMock {
      val mockData = buildDefaultMockData()

      val headers: NonEmptyChain[BlockHeader] =
        nonEmptyChainArbOf(arbitraryHeader).arbitrary.retryUntil(_.size < maxChainSize).first.map(_.embedId)
      val idAndHeader: NonEmptyChain[(BlockId, BlockHeader)] =
        headers.map(h => (h.id, h))
      val idToHeader: Map[BlockId, BlockHeader] = idAndHeader.toList.toMap
      val blocksCount = idToHeader.size

      (mockData.client
        .getHeaderOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .rep(blocksCount)
        .onCall { case (id: BlockId, e: Throwable @unchecked, _: MonadThrow[F] @unchecked) =>
          OptionT(idToHeader.get(id).pure[F]).getOrRaise(e)
        }

      idToHeader.foreach { case (id, header) =>
        (mockData.blockIdTree.associate).expects(id, header.parentHeaderId).returns(().pure[F])
      }

      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.DownloadHeadersResponse(
          hostId,
          idAndHeader.map { case (id, header) => (id, Either.right(UnverifiedBlockHeader(hostId, header, 0))) }
        )

      (mockData.requestsProxy.sendNoWait)
        .expects(compareDownloadedHeaderWithoutDownloadTimeMatcher(expectedMessage))
        .once()
        .returning(().pure[F])

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            _ <- actor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(idAndHeader.map(_._1)))
          } yield ()
        }
    }
  }

  test("One block header shall be downloaded by request proper with delay measurements") {
    withMock {
      val pingDelay = 90

      val mockData = buildDefaultMockData()

      val header: BlockHeader = arbitraryHeader.arbitrary.first.embedId

      (mockData.client
        .getHeaderOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(header.id, *, *)
        .once()
        .returns(Async[F].delayBy(header.pure[F], FiniteDuration(pingDelay, MILLISECONDS)))

      (mockData.blockIdTree.associate).expects(header.id, header.parentHeaderId).returns(().pure[F])

      (mockData.requestsProxy.sendNoWait)
        .expects(*)
        .once()
        .onCall { (message: RequestsProxy.Message) =>
          message match {
            case RequestsProxy.Message.DownloadHeadersResponse(`hostId`, response) =>
              if (response.head._2.forall(b => b.downloadTimeMs < pingDelay || b.downloadTimeMs > pingDelay + 30))
                throw new IllegalStateException()
              else
                ().pure[F]
            case _ => throw new IllegalStateException()
          }
        }

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            _ <- actor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(NonEmptyChain.one(header.id)))
          } yield ()
        }
    }
  }

  test("Block header shall be downloaded by request, for missing headers error shall be returned") {
    withMock {
      val mockData = buildDefaultMockData()

      val headers: NonEmptyChain[BlockHeader] =
        nonEmptyChainArbOf(arbitraryHeader).arbitrary.retryUntil(_.size < maxChainSize).first.map(_.embedId)
      val idAndHeader: NonEmptyChain[(BlockId, BlockHeader)] =
        headers.map(h => (h.id, h))

      def missedBlockId(id: BlockId): Boolean = (id.hashCode() % 2) == 0

      val idToHeaderOnClient: Map[BlockId, BlockHeader] = idAndHeader.toList.filterNot(d => missedBlockId(d._1)).toMap

      idToHeaderOnClient.foreach { case (id, header) =>
        (mockData.blockIdTree.associate).expects(id, header.parentHeaderId).returns(().pure[F])
      }

      (mockData.client
        .getHeaderOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .rep(idAndHeader.size.toInt)
        .onCall { case (id: BlockId, e: Throwable, _: MonadThrow[F] @unchecked) =>
          OptionT(idToHeaderOnClient.get(id).pure[F]).getOrRaise(e)
        }

      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.DownloadHeadersResponse(
          hostId,
          idAndHeader.map { case (id, header) =>
            if (missedBlockId(id)) {
              (id, Either.left(HeaderNotFoundInPeer))
            } else {
              (id, Either.right(UnverifiedBlockHeader(hostId, header, 0)))
            }
          }
        )
      (mockData.requestsProxy.sendNoWait)
        .expects(compareDownloadedHeaderWithoutDownloadTimeMatcher(expectedMessage))
        .once()
        .returning(().pure[F])

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            _ <- actor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(idAndHeader.map(_._1)))
          } yield ()
        }
    }
  }

  test("Block header shall be downloaded by request, incorrect headers shall be skipped") {
    withMock {
      val mockData = buildDefaultMockData()

      val headers: NonEmptyChain[BlockHeader] =
        nonEmptyChainArbOf(arbitraryHeader).arbitrary.retryUntil(_.size < maxChainSize).first.map(_.embedId)
      val idAndHeader: NonEmptyChain[(BlockId, BlockHeader)] =
        headers.map(h => (h.id, h))

      def missedBlockId(id: BlockId): Boolean = (id.hashCode() % 2) == 0

      val idToHeaderOnClient: Map[BlockId, BlockHeader] = idAndHeader.toList.filterNot(d => missedBlockId(d._1)).toMap
      val incorrectHeader = arbitraryHeader.arbitrary.first
      val incorrectHeaderId = incorrectHeader.id
      (mockData.client
        .getHeaderOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .rep(idAndHeader.size.toInt)
        .onCall { case (id: BlockId, e: Throwable, _: MonadThrow[F] @unchecked) =>
          if (missedBlockId(id)) {
            incorrectHeader.pure[F]
          } else {
            OptionT(idToHeaderOnClient.get(id).pure[F]).getOrRaise(e)
          }
        }

      idToHeaderOnClient.foreach { case (id, header) =>
        (mockData.blockIdTree.associate).expects(id, header.parentHeaderId).returns(().pure[F])
      }

      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.DownloadHeadersResponse(
          hostId,
          idAndHeader.map { case (id, header) =>
            if (missedBlockId(id)) {
              (id, Either.left(HeaderHaveIncorrectId(id, incorrectHeaderId)))
            } else {
              (id, Either.right(UnverifiedBlockHeader(hostId, header, 0)))
            }
          }
        )
      (mockData.requestsProxy.sendNoWait)
        .expects(compareDownloadedHeaderWithoutDownloadTimeMatcher(expectedMessage))
        .once()
        .returning(().pure[F])

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            _ <- actor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(idAndHeader.map(_._1)))
          } yield ()
        }
    }
  }

  def prepareSlotDataStorage(
    slotDataStoreMap: mutable.Map[BlockId, SlotData],
    mockData:         PeerBlockHeaderMockData
  ): Unit = {
    (mockData.slotDataStore.get).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
      slotDataStoreMap.get(id).pure[F]
    }
    (mockData.slotDataStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
      slotDataStoreMap.contains(id).pure[F]
    }
    (mockData.slotDataStore
      .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
      .expects(*, *, *)
      .anyNumberOfTimes()
      .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
        slotDataStoreMap(id).pure[F]
      }
  }

  def clientDownloadRemoteSlotDataChain(
    mockData:              PeerBlockHeaderMockData,
    localBestSlotData:     SlotData,
    responseIdAndSlotData: List[(BlockId, SlotData)],
    remoteIdToSlotData:    Map[BlockId, SlotData]
  ): Unit = {
    val localBestTip: BlockId = localBestSlotData.slotId.blockId
    val remoteSlotData = responseIdAndSlotData.map(_._2)

    // long chain sync as result

    (mockData.commonAncestorF.apply).expects(*, *).returning(localBestTip.pure[F])

    val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
    (mockData.client.getRemoteBlockIdAtHeight).expects(*).anyNumberOfTimes().onCall { (height: Long) =>
      remoteHeightToBlockId.get(height).pure[F]
    }

    (mockData.client
      .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
      .expects(*, *, *)
      .anyNumberOfTimes()
      .onCall { case (id: BlockId, e: Throwable @unchecked, _: MonadThrow[F] @unchecked) =>
        OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e)
      }

    (mockData.client.getRemoteSlotDataWithParents)
      .expects(localBestTip, remoteSlotData.init.lastOption.get.slotId.blockId)
      .once()
      .returns(remoteSlotData.init.prepended(localBestSlotData).some.pure[F])
  }

  test("New better slot data and block source shall be sent if local chain is worse: one slot") {
    withMock {
      val mockData = buildDefaultMockData()

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(1L, 1L)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, knownSlotData) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val (bestSlotId, bestSlotData) = remoteSlotData.last
      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap
      val remoteSlotDataCount = remoteIdToSlotData.size

      (() => mockData.client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }

      val expectedSourceMessage: PeersManager.Message =
        PeersManager.Message.BlocksSource(remoteSlotData.map(d => (hostId, d._1)))
      (mockData.peersManager.sendNoWait).expects(expectedSourceMessage).once().returning(().pure[F])
      val expectedSlotDataMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (mockData.requestsProxy.sendNoWait).expects(expectedSlotDataMessage).once().returning(().pure[F])

      (() => mockData.localChain.head).expects().anyNumberOfTimes().returns(bestSlotData.pure[F])
      (mockData.localChain.isWorseThan).expects(*).once().onCall { (ids: NonEmptyChain[SlotData]) =>
        assert(bestSlotData == ids.last)
        (bestSlotData == ids.last).pure[F]
      }
      (mockData.bodyStore.contains).expects(remoteSlotData.last._1).once().returns(false.pure[F])
      (mockData.bodyStore.contains).expects(remoteSlotData.last._2.parentSlotId.blockId).once().returns(true.pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(knownId, knownSlotData)
      (mockData.slotDataStore.put).expects(*, *).rep(remoteSlotDataCount).onCall {
        case (id: BlockId, slotData: SlotData) =>
          slotDataStoreMap.put(id, slotData).pure[F].void
      }
      prepareSlotDataStorage(slotDataStoreMap, mockData)

      val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
      (mockData.client.getRemoteBlockIdAtHeight).expects(*).anyNumberOfTimes().onCall { (height: Long) =>
        remoteHeightToBlockId.get(height).pure[F]
      }

      (mockData.client
        .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: Throwable, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e)
        }

      (() => mockData.clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New better slot data and block source shall be sent if local chain is worse: Long chain") {
    withMock {
      val mockData = buildDefaultMockData()

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, knownSlotData) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val (bestSlotId, bestSlotData) = remoteSlotData.last
      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap
      val remoteSlotDataCount = remoteIdToSlotData.size

      (() => mockData.client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }

      val expectedSourceMessage: PeersManager.Message =
        PeersManager.Message.BlocksSource(remoteSlotData.map(d => (hostId, d._1)))
      (mockData.peersManager.sendNoWait).expects(expectedSourceMessage).once().returning(().pure[F])
      val expectedSlotDataMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (mockData.requestsProxy.sendNoWait).expects(expectedSlotDataMessage).once().returning(().pure[F])

      (() => mockData.localChain.head).expects().anyNumberOfTimes().returns(bestSlotData.pure[F])
      (mockData.localChain.isWorseThan).expects(*).once().onCall { (ids: NonEmptyChain[SlotData]) =>
        assert(bestSlotData == ids.last)
        (bestSlotData == ids.last).pure[F]
      }
      (mockData.bodyStore.contains).expects(remoteSlotData.last._1).once().returns(false.pure[F])
      (mockData.bodyStore.contains).expects(remoteSlotData.last._2.parentSlotId.blockId).once().returns(false.pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(knownId, knownSlotData)
      (mockData.slotDataStore.put).expects(*, *).rep(remoteSlotDataCount).onCall {
        case (id: BlockId, slotData: SlotData) =>
          slotDataStoreMap.put(id, slotData).pure[F].void
      }
      prepareSlotDataStorage(slotDataStoreMap, mockData)
      clientDownloadRemoteSlotDataChain(mockData, knownSlotData, remoteSlotData.toList, remoteSlotData.toList.toMap)

      (() => mockData.clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New better slot data and block source shall be sent chunk if local chain is worse") {
    withMock {
      val enoughHeightDelta = 1
      val chainSelection: ChainSelectionAlgebra[F, BlockId, SlotData] =
        new ChainSelectionAlgebra[F, BlockId, SlotData] {

          override def compare(
            x:        SlotData,
            y:        SlotData,
            xFetcher: BlockId => F[SlotData],
            yFetcher: BlockId => F[SlotData]
          ): F[Int] =
            x.height.compare(y.height).pure[F]

          override def enoughHeightToCompare(currentHeight: Long, commonHeight: Long, proposedHeight: Long): F[Long] =
            (proposedHeight - enoughHeightDelta).pure[F]
        }
      val mockData = buildDefaultMockData(chainSelection = chainSelection)

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, knownSlotData) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val (bestSlotId, bestSlotData) = remoteSlotData.last
      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap
      val remoteSlotDataCount = remoteIdToSlotData.size

      (() => mockData.client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }

      (() => mockData.localChain.head).expects().anyNumberOfTimes().returns(bestSlotData.pure[F])
      val req = remoteSlotData.toList.dropRight(enoughHeightDelta)
      (mockData.localChain.isWorseThan).expects(*).once().onCall { (ids: NonEmptyChain[SlotData]) =>
        assert(req.last._2 == ids.last)
        (req.last._2 == ids.last).pure[F]
      }

      val necMes = NonEmptyChain.fromSeq(req).get
      val expectedSourceMessage: PeersManager.Message =
        PeersManager.Message.BlocksSource(necMes.map(d => (hostId, d._1)))
      (mockData.peersManager.sendNoWait).expects(expectedSourceMessage).once().returning(().pure[F])

      val expectedSlotDataMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, necMes.map(_._2))
      (mockData.requestsProxy.sendNoWait).expects(expectedSlotDataMessage).once().returning(().pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(knownId, knownSlotData)
      (mockData.slotDataStore.put).expects(*, *).rep(remoteSlotDataCount - enoughHeightDelta).onCall {
        case (id: BlockId, slotData: SlotData) =>
          slotDataStoreMap.put(id, slotData).pure[F].void
      }

      (mockData.bodyStore.contains).expects(remoteSlotData.last._1).once().returns(false.pure[F])
      (mockData.bodyStore.contains).expects(remoteSlotData.last._2.parentSlotId.blockId).once().returns(false.pure[F])

      prepareSlotDataStorage(slotDataStoreMap, mockData)
      clientDownloadRemoteSlotDataChain(
        mockData,
        knownSlotData,
        remoteSlotData.toList.dropRight(enoughHeightDelta),
        remoteSlotData.toList.toMap
      )

      (() => mockData.clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New better slot data and block source shall be sent chunk if local chain is worse but we have slot data") {
    withMock {
      val enoughHeightDelta = 1
      val chainSelection: ChainSelectionAlgebra[F, BlockId, SlotData] =
        new ChainSelectionAlgebra[F, BlockId, SlotData] {

          override def compare(
            x:        SlotData,
            y:        SlotData,
            xFetcher: BlockId => F[SlotData],
            yFetcher: BlockId => F[SlotData]
          ): F[Int] =
            x.height.compare(y.height).pure[F]

          override def enoughHeightToCompare(currentHeight: Long, commonHeight: Long, proposedHeight: Long): F[Long] =
            (proposedHeight - enoughHeightDelta).pure[F]
        }
      val mockData = buildDefaultMockData(chainSelection = chainSelection)

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(5, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, knownSlotData) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val (bestSlotId, bestSlotData) = remoteSlotData.last
      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap

      (() => mockData.client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }
      val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
      (mockData.client.getRemoteBlockIdAtHeight).expects(*).onCall { (height: Long) =>
        remoteHeightToBlockId.get(height).pure[F]
      }

      (() => mockData.localChain.head).expects().anyNumberOfTimes().returns(bestSlotData.pure[F])
      val req = remoteSlotData.toList.dropRight(enoughHeightDelta)

      (mockData.localChain.isWorseThan).expects(*).once().onCall { (ids: NonEmptyChain[SlotData]) =>
        assert(req.last._2 == ids.last)
        (req.last._2 == ids.last).pure[F]
      }

      val necMes = NonEmptyChain.fromSeq(req).get
      val expectedSourceMessage: PeersManager.Message =
        PeersManager.Message.BlocksSource(necMes.map(d => (hostId, d._1)))
      (mockData.peersManager.sendNoWait).expects(expectedSourceMessage).once().returning(().pure[F])

      val expectedSlotDataMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, necMes.map(_._2))
      (mockData.requestsProxy.sendNoWait).expects(expectedSlotDataMessage).once().returning(().pure[F])

      val slotDataStoreMap = mutable.Map.from(slotData.map(sd => sd.slotId.blockId -> sd).toList.toMap)

      (() => mockData.clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      (mockData.bodyStore.contains).expects(*).anyNumberOfTimes().onCall((id: BlockId) => (knownId == id).pure[F])

      prepareSlotDataStorage(slotDataStoreMap, mockData)
      clientDownloadRemoteSlotDataChain(
        mockData,
        knownSlotData,
        remoteSlotData.toList.dropRight(enoughHeightDelta),
        remoteSlotData.toList.toMap
      )

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New slot data shall not be sent if local chain is better by density and block source is not interesting") {
    withMock {
      val mockData = buildDefaultMockData()

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(5, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, knownSlotData) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val (bestSlotId, bestSlotData) = remoteSlotData.last
      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap

      (() => mockData.client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }
      remoteIdToSlotData.map { case (id, sd) => sd.height -> id }

      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (mockData.requestsProxy.sendNoWait).expects(expectedMessage).never()
      (mockData.requestsProxy.sendNoWait)
        .expects(RequestsProxy.Message.BadKLookbackSlotData(hostId))
        .once()
        .returning(Applicative[F].unit)

      (mockData.localChain.isWorseThan).expects(*).once().onCall { (ids: NonEmptyChain[SlotData]) =>
        assert(bestSlotData == ids.last)
        false.pure[F]
      }

      (() => mockData.localChain.head).expects().anyNumberOfTimes().returning(knownSlotData.pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(knownId, knownSlotData)
      (mockData.slotDataStore.put)
        .expects(*, *)
        .rep(remoteSlotData.size.toInt)
        .onCall { case (id: BlockId, slotData: SlotData) =>
          slotDataStoreMap.put(id, slotData).pure[F].void
        }

      (() => mockData.clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      (mockData.bodyStore.contains).expects(*).anyNumberOfTimes().onCall((id: BlockId) => (knownId == id).pure[F])

      prepareSlotDataStorage(slotDataStoreMap, mockData)
      clientDownloadRemoteSlotDataChain(
        mockData,
        knownSlotData,
        remoteSlotData.toList,
        remoteSlotData.toList.toMap
      )

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New better slot data and block source shall be NOT be sent if we still process previous slot data") {
    withMock {
      val mockData = buildDefaultMockData()

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2L, 2L)).arbitrary.first
      val idAndSlotData = slotData.map(s => (s.slotId.blockId, s)).toList

      val (localId, localSlot) = idAndSlotData.head
      val (remote1Id, remote1Slot) = idAndSlotData(1)
      val (remote2Id, _) = idAndSlotData(2)
      val remoteSlotData = idAndSlotData.tail

      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toMap

      (mockData.client
        .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: Throwable, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e)
        }
      (() => mockData.client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.emits(Seq(remote1Id, remote2Id)).covary.pure[F]
      }

      (mockData.peersManager.sendNoWait).expects(*).once().returning(().pure[F])
      (mockData.requestsProxy.sendNoWait).expects(*).once().returning(().pure[F])

      (() => mockData.localChain.head).expects().anyNumberOfTimes().returns(localSlot.pure[F])
      (mockData.localChain.isWorseThan).expects(*).once().onCall { (ids: NonEmptyChain[SlotData]) =>
        assert(remote1Slot == ids.last)
        (remote1Slot == ids.last).pure[F]
      }

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(localId, localSlot)
      (mockData.slotDataStore.get).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        slotDataStoreMap.get(id).pure[F]
      }
      (mockData.slotDataStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        slotDataStoreMap.contains(id).pure[F]
      }
      (mockData.slotDataStore.put).expects(*, *).once().onCall { case (id: BlockId, slotData: SlotData) =>
        slotDataStoreMap.put(id, slotData).pure[F].void
      }
      (mockData.slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreMap(id).pure[F]
        }

      (() => mockData.clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      (mockData.bodyStore.contains).expects(*).anyNumberOfTimes().onCall((id: BlockId) => (localId == id).pure[F])

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New better slot data and block source shall be PROCESSED after we process previous slot data") {
    withMock {
      val mockData = buildDefaultMockData()

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2L, 2L)).arbitrary.first
      val idAndSlotData = slotData.map(s => (s.slotId.blockId, s)).toList

      val (localId, localSlot) = idAndSlotData.head
      val (remote1Id, remote1Slot) = idAndSlotData(1)
      val (remote2Id, remote2Slot) = idAndSlotData(2)
      val remoteSlotData = idAndSlotData.tail

      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toMap

      (mockData.client
        .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: Throwable, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e)
        }
      (() => mockData.client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.emits(Seq(remote1Id, remote2Id)).covary.pure[F]
      }

      (mockData.peersManager.sendNoWait).expects(*).twice().returning(().pure[F])
      (mockData.requestsProxy.sendNoWait).expects(*).twice().returning(().pure[F])

      (() => mockData.localChain.head).expects().once().returns(localSlot.pure[F])
      (() => mockData.localChain.head).expects().once().returns(remote1Slot.pure[F])
      (mockData.localChain.isWorseThan).expects(*).once().onCall { (ids: NonEmptyChain[SlotData]) =>
        assert(remote1Slot == ids.last)
        (remote1Slot == ids.last).pure[F]
      }
      (mockData.localChain.isWorseThan).expects(*).once().onCall { (ids: NonEmptyChain[SlotData]) =>
        assert(remote2Slot == ids.last)
        (remote2Slot == ids.last).pure[F]
      }

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(localId, localSlot)
      (mockData.slotDataStore.get).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        slotDataStoreMap.get(id).pure[F]
      }
      (mockData.slotDataStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        slotDataStoreMap.contains(id).pure[F]
      }
      (mockData.slotDataStore.put).expects(*, *).twice().onCall { case (id: BlockId, slotData: SlotData) =>
        slotDataStoreMap.put(id, slotData).pure[F].void
      }
      (mockData.slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreMap(id).pure[F]
        }

      (() => mockData.clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      (mockData.bodyStore.contains).expects(remote1Id).once().returning(false.pure[F])
      (mockData.bodyStore.contains).expects(localId).once().returning(true.pure[F])

      (mockData.bodyStore.contains).expects(remote2Id).once().returning(false.pure[F])
      (mockData.bodyStore.contains).expects(remote1Id).once().returning(true.pure[F])

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New slot data shall not be sent if we receive known worse block") {
    withMock {
      val mockData = buildDefaultMockData()

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, knownSlotData) = idAndSlotData.last
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.init).get

      val (bestSlotId, _) = remoteSlotData.last
      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap

      (mockData.client
        .getSlotDataOrError(_: BlockId, _: BlockHeaderDownloadError)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: BlockHeaderDownloadError @unchecked, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e)
        }
      (() => mockData.client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }
      val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
      (mockData.client.getRemoteBlockIdAtHeight).expects(*).onCall { (height: Long) =>
        remoteHeightToBlockId.get(height).pure[F]
      }

      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (mockData.requestsProxy.sendNoWait).expects(expectedMessage).never()

      (() => mockData.localChain.head).expects().anyNumberOfTimes().returning(knownSlotData.pure[F])

      val slotDataStoreMap = idAndSlotData.map { case (id, slot) => id -> slot }.toList.toMap

      (mockData.slotDataStore.get).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        slotDataStoreMap.get(id).pure[F]
      }
      (mockData.slotDataStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        slotDataStoreMap.contains(id).pure[F]
      }

      (mockData.slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreMap(id).pure[F]
        }

      (() => mockData.clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      (mockData.commonAncestorF.apply).expects(*, *).returning(bestSlotId.pure[F])

      (mockData.bodyStore.contains).expects(*).anyNumberOfTimes().onCall((id: BlockId) => (knownId == id).pure[F])

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New slot data shall not be sent if we receive unknown worse block: build long chain") {
    withMock {
      val mockData = buildDefaultMockData()

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(3, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (_, knownSlotData) = idAndSlotData.last
      val remoteSlotData =
        arbitraryLinkedSlotDataChainFor(Gen.choose(1L, 1L), idAndSlotData.head._2.some).arbitrary.first
          .map(sd => sd.slotId.blockId -> sd)

      val (bestSlotId, bestSlotData) = remoteSlotData.last
      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap

      (() => mockData.client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }
      val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
      (mockData.client.getRemoteBlockIdAtHeight).expects(*).onCall { (height: Long) =>
        remoteHeightToBlockId.get(height).pure[F]
      }

      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (mockData.requestsProxy.sendNoWait).expects(expectedMessage).never()

      (() => mockData.localChain.head).expects().anyNumberOfTimes().returning(knownSlotData.pure[F])
      (mockData.localChain.isWorseThan).expects(*).once().onCall { (ids: NonEmptyChain[SlotData]) =>
        assert(bestSlotData == ids.last)
        (bestSlotData == ids.last).pure[F]
      }

      val slotDataStoreMap = mutable.Map() ++ idAndSlotData.map { case (id, slot) => id -> slot }.toList.toMap
      (mockData.slotDataStore.put)
        .expects(*, *)
        .rep(remoteSlotData.size.toInt)
        .onCall { case (id: BlockId, slotData: SlotData) =>
          slotDataStoreMap.put(id, slotData).pure[F].void
        }

      (() => mockData.clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      val knownBodies = idAndSlotData.map(_._1).toList.toSet
      (mockData.bodyStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        knownBodies.contains(id).pure[F]
      }

      prepareSlotDataStorage(slotDataStoreMap, mockData)
      clientDownloadRemoteSlotDataChain(
        mockData,
        idAndSlotData.head._2,
        remoteSlotData.toList,
        remoteSlotData.toList.toMap
      )

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New slot data shall not be sent if we receive unknown worse block: one block chain") {
    withMock {
      val mockData = buildDefaultMockData()

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(3, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (_, knownSlotData) = idAndSlotData.last
      val remoteSlotData =
        arbitraryLinkedSlotDataChainFor(Gen.choose(0L, 0L), idAndSlotData.head._2.some).arbitrary.first
          .map(sd => sd.slotId.blockId -> sd)

      val (bestSlotId, bestSlotData) = remoteSlotData.last
      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap

      (mockData.client
        .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: Throwable, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e)
        }
      (() => mockData.client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }

      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (mockData.requestsProxy.sendNoWait).expects(expectedMessage).never()

      (() => mockData.localChain.head).expects().anyNumberOfTimes().returning(knownSlotData.pure[F])
      (mockData.localChain.isWorseThan).expects(*).once().onCall { (ids: NonEmptyChain[SlotData]) =>
        assert(bestSlotData == ids.last)
        (bestSlotData == ids.last).pure[F]
      }

      val slotDataStoreMap = mutable.Map() ++ idAndSlotData.map { case (id, slot) => id -> slot }.toList.toMap

      (mockData.slotDataStore.get).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        slotDataStoreMap.get(id).pure[F]
      }
      (mockData.slotDataStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        slotDataStoreMap.contains(id).pure[F]
      }
      (mockData.slotDataStore.put)
        .expects(*, *)
        .rep(remoteSlotData.size.toInt)
        .onCall { case (id: BlockId, slotData: SlotData) =>
          slotDataStoreMap.put(id, slotData).pure[F].void
        }

      (mockData.slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreMap(id).pure[F]
        }

      (() => mockData.clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      val knownBodies = idAndSlotData.map(_._1).toList.toSet
      (mockData.bodyStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        knownBodies.contains(id).pure[F]
      }

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("Sent only block source if we receive current head") {
    withMock {
      val mockData = buildDefaultMockData()

      val slotData = arbitrarySlotData.arbitrary.first

      (() => mockData.client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](slotData.slotId.blockId.pure[F]).pure[F]
      }

      (mockData.peersManager.sendNoWait)
        .expects(PeersManager.Message.BlocksSource(NonEmptyChain.one(hostId -> slotData.slotId.blockId)))
        .returning(Applicative[F].unit)

      (() => mockData.localChain.head).expects().anyNumberOfTimes().returning(slotData.pure[F])

      (mockData.slotDataStore.get)
        .expects(slotData.slotId.blockId)
        .anyNumberOfTimes()
        .returning(slotData.some.pure[F])
      (mockData.slotDataStore.contains)
        .expects(slotData.parentSlotId.blockId)
        .anyNumberOfTimes()
        .returning(true.pure[F])
      (mockData.slotDataStore.contains).expects(slotData.slotId.blockId).anyNumberOfTimes().returning(true.pure[F])
      (mockData.slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(slotData.slotId.blockId, *, *)
        .anyNumberOfTimes()
        .returning(slotData.pure[F])

      (() => mockData.clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      (mockData.bodyStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        (slotData.slotId.blockId == id).pure[F]
      }

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }

    }
  }

  test("Sent only block source if we receive worse block with same height") {
    withMock {
      val mockData = buildDefaultMockData()
      val commonAncestor = arbitrarySlotData.arbitrary.first.copy(height = 5)
      val thisHead = arbitrarySlotData.arbitrary.first.copy(parentSlotId = commonAncestor.slotId, height = 6)
      val remoteKnownHead = arbitrarySlotData.arbitrary.first.copy(parentSlotId = commonAncestor.slotId, height = 6)

      (() => mockData.client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](remoteKnownHead.slotId.blockId.pure[F]).pure[F]
      }

      (mockData.peersManager.sendNoWait)
        .expects(PeersManager.Message.BlocksSource(NonEmptyChain.one(hostId -> remoteKnownHead.slotId.blockId)))
        .returning(Applicative[F].unit)

      (() => mockData.localChain.head).expects().anyNumberOfTimes().returning(thisHead.pure[F])

      (mockData.slotDataStore.get)
        .expects(remoteKnownHead.slotId.blockId)
        .anyNumberOfTimes()
        .returning(remoteKnownHead.some.pure[F])
      (mockData.slotDataStore.contains)
        .expects(remoteKnownHead.parentSlotId.blockId)
        .anyNumberOfTimes()
        .returning(true.pure[F])
      (mockData.slotDataStore.contains)
        .expects(remoteKnownHead.slotId.blockId)
        .anyNumberOfTimes()
        .returning(true.pure[F])
      (mockData.slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(remoteKnownHead.slotId.blockId, *, *)
        .anyNumberOfTimes()
        .returning(remoteKnownHead.pure[F])

      (() => mockData.clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      (mockData.bodyStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        (remoteKnownHead.slotId.blockId == id).pure[F]
      }

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }

    }
  }

  test("Requested tip shall be sent if local chain is worse") {
    withMock {
      val mockData = buildDefaultMockData()

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, knownSlotData) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap

      val bestTip = idAndSlotData.last._2

      ((() => mockData.client.remoteCurrentTip()))
        .expects()
        .returns(Option(bestTip.slotId.blockId).pure[F])
      (() => mockData.client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.fromOption[F](Option.empty[BlockId]).pure[F]
      }

      val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
      (mockData.client.getRemoteBlockIdAtHeight).expects(*).onCall { (height: Long) =>
        remoteHeightToBlockId.get(height).pure[F]
      }

      val expectedSourceMessage: PeersManager.Message =
        PeersManager.Message.BlocksSource(remoteSlotData.map(d => (hostId, d._1)))
      (mockData.peersManager.sendNoWait).expects(expectedSourceMessage).once().returning(().pure[F])
      val expectedSlotDataMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (mockData.requestsProxy.sendNoWait).expects(expectedSlotDataMessage).once().returning(().pure[F])

      (mockData.localChain.isWorseThan).expects(*).once().onCall { (ids: NonEmptyChain[SlotData]) =>
        assert(bestTip == ids.last)
        (bestTip == ids.last).pure[F]
      }
      (() => mockData.localChain.head).expects().anyNumberOfTimes().returning(knownSlotData.pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(knownId, knownSlotData)
      (mockData.slotDataStore.put).expects(*, *).anyNumberOfTimes().onCall { case (id: BlockId, slotData: SlotData) =>
        slotDataStoreMap.put(id, slotData).pure[F].void
      }

      (() => mockData.clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      (mockData.bodyStore.contains).expects(*).anyNumberOfTimes().onCall((id: BlockId) => (knownId == id).pure[F])

      prepareSlotDataStorage(slotDataStoreMap, mockData)
      clientDownloadRemoteSlotDataChain(
        mockData,
        knownSlotData,
        remoteSlotData.toList,
        remoteSlotData.toList.toMap
      )

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            _     <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            state <- actor.send(PeerBlockHeaderFetcher.Message.GetCurrentTip)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("Bad KLookback shall be send because tip could be better after full check") {
    withMock {
      val mockData = buildDefaultMockData()

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, knownSlotData) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap

      val bestTip = idAndSlotData.last._2

      ((() => mockData.client.remoteCurrentTip()))
        .expects()
        .returns(Option(bestTip.slotId.blockId).pure[F])
      (() => mockData.client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.fromOption[F](Option.empty[BlockId]).pure[F]
      }
      val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
      (mockData.client.getRemoteBlockIdAtHeight).expects(*).onCall { (height: Long) =>
        remoteHeightToBlockId.get(height).pure[F]
      }

      val expectedBadKLookbackMessage: RequestsProxy.Message = RequestsProxy.Message.BadKLookbackSlotData(hostId)
      (mockData.requestsProxy.sendNoWait).expects(expectedBadKLookbackMessage).once().returning(().pure[F])

      (mockData.localChain.isWorseThan).expects(*).once().onCall { (ids: NonEmptyChain[SlotData]) =>
        assert(bestTip == ids.last)
        false.pure[F]
      }
      val head = arbitrarySlotData.arbitrary.first.copy(height = 1)
      (() => mockData.localChain.head).expects().anyNumberOfTimes().returning(head.pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(knownId, knownSlotData)
      (mockData.slotDataStore.put).expects(*, *).anyNumberOfTimes().onCall { case (id: BlockId, slotData: SlotData) =>
        slotDataStoreMap.put(id, slotData).pure[F].void
      }

      (() => mockData.clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      (mockData.bodyStore.contains).expects(*).anyNumberOfTimes().onCall((id: BlockId) => (knownId == id).pure[F])
      prepareSlotDataStorage(slotDataStoreMap, mockData)
      clientDownloadRemoteSlotDataChain(
        mockData,
        knownSlotData,
        remoteSlotData.toList,
        remoteSlotData.toList.toMap
      )

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            _     <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            state <- actor.send(PeerBlockHeaderFetcher.Message.GetCurrentTip)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("Requested tip shall not be sent if local chain is better") {
    withMock {
      val mockData = buildDefaultMockData()

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, knownSlotData) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap

      val bestTip = idAndSlotData.last._2

      ((() => mockData.client.remoteCurrentTip()))
        .expects()
        .returns(Option(bestTip.slotId.blockId).pure[F])
      (() => mockData.client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.fromOption[F](Option.empty[BlockId]).pure[F]
      }

      val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
      (mockData.client.getRemoteBlockIdAtHeight).expects(*).onCall { (height: Long) =>
        remoteHeightToBlockId.get(height).pure[F]
      }

      val expectedMessage: RequestsProxy.Message = RequestsProxy.Message.BadKLookbackSlotData(hostId)
      (mockData.requestsProxy.sendNoWait).expects(expectedMessage).once().returns(().pure[F])

      (() => mockData.localChain.head).expects().anyNumberOfTimes().returning(knownSlotData.pure[F])
      (mockData.localChain.isWorseThan).expects(*).once().onCall { (ids: NonEmptyChain[SlotData]) =>
        assert(bestTip == ids.last)
        false.pure[F]
      }

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(knownId, knownSlotData)
      (mockData.slotDataStore.put).expects(*, *).anyNumberOfTimes().onCall { case (id: BlockId, slotData: SlotData) =>
        slotDataStoreMap.put(id, slotData).pure[F].void
      }

      (() => mockData.clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      (mockData.bodyStore.contains).expects(*).anyNumberOfTimes().onCall((id: BlockId) => (knownId == id).pure[F])

      prepareSlotDataStorage(slotDataStoreMap, mockData)
      clientDownloadRemoteSlotDataChain(
        mockData,
        knownSlotData,
        remoteSlotData.toList,
        remoteSlotData.toList.toMap
      )

      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            _     <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            state <- actor.send(PeerBlockHeaderFetcher.Message.GetCurrentTip)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }

    }
  }

  test("New slot data should be rejected if the genesis timestamp has not elapsed") {
    withMock {
      val mockData = buildDefaultMockData()

      val slotData = arbitrarySlotData.arbitrary.first.update(_.slotId.slot.set(5L))

      (mockData.peersManager.sendNoWait)
        .expects(PeersManager.Message.NonCriticalErrorForHost(hostId))
        .returns(Applicative[F].unit)

      (() => mockData.client.remotePeerAdoptions)
        .expects()
        .once()
        .returning(Stream(slotData.slotId.blockId).covaryAll[F, BlockId].pure[F])
      (mockData.client
        .getSlotDataOrError[Throwable](_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(slotData.slotId.blockId, *, *)
        .once()
        .returning(slotData.pure[F])

      (mockData.slotDataStore
        .get(_: BlockId))
        .expects(slotData.slotId.blockId)
        .once()
        .returning(none[SlotData].pure[F])

      (() => mockData.clock.globalSlot)
        .expects()
        .once()
        .returning((-1L).pure[F])

      (mockData.bodyStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        (slotData.slotId.blockId == id).pure[F]
      }

      buildActorFromMockData(mockData).use { actor =>
        for {
          _ <- Sync[F].andWait(
            actor.sendNoWait(PeerBlockHeaderFetcher.Message.StartActor),
            FiniteDuration(100, MILLISECONDS)
          )
        } yield ()
      }
    }
  }
}
