package org.plasmalabs.networking.fsnetwork

import cats.data.*
import cats.effect.{IO, Resource}
import cats.implicits.*
import cats.{MonadThrow, Show}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.algebras.Store
import org.plasmalabs.blockchain.{Validators, ValidatorsImpl}
import org.plasmalabs.codecs.bytes.tetra.instances.*
import org.plasmalabs.config.ApplicationConfig.Node.NetworkProperties
import org.plasmalabs.consensus.*
import org.plasmalabs.consensus.algebras.*
import org.plasmalabs.consensus.models.BlockHeaderValidationFailures.NonForwardSlot
import org.plasmalabs.consensus.models.{BlockHeader, BlockHeaderValidationFailure, BlockId, *}
import org.plasmalabs.crypto.signing.Ed25519VRF
import org.plasmalabs.ledger.algebras.*
import org.plasmalabs.ledger.models.*
import org.plasmalabs.ledger.models.BodySemanticErrors.TransactionSemanticErrors
import org.plasmalabs.ledger.models.TransactionSemanticErrors.InputDataMismatch
import org.plasmalabs.models.ModelGenerators.GenHelper
import org.plasmalabs.models.generators.consensus.ModelGenerators.*
import org.plasmalabs.models.generators.node.ModelGenerators
import org.plasmalabs.models.p2p.*
import org.plasmalabs.networking.fsnetwork.BlockCheckerTest.F
import org.plasmalabs.networking.fsnetwork.PeersManager.PeersManagerActor
import org.plasmalabs.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import org.plasmalabs.networking.fsnetwork.TestHelper.*
import org.plasmalabs.node.models.{Block, BlockBody}
import org.plasmalabs.quivr.runtime.DynamicContext
import org.plasmalabs.sdk.models.Datum
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.validation.algebras.{TransactionAuthorizationVerifier, TransactionSyntaxVerifier}
import org.plasmalabs.typeclasses.implicits.*
import org.scalacheck.Gen
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.mutable
import scala.concurrent.duration.*

object BlockCheckerTest {
  type F[A] = IO[A]
}

class BlockCheckerTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)
  implicit val ed255Vrf: Ed25519VRF = Ed25519VRF.precomputed()
  private val hostId: HostId = arbitraryHost.arbitrary.first
  private val maxChainSize = 5
  private val chunkSize = 1
  private val defaultConfig = P2PNetworkConfig(NetworkProperties(chunkSize = chunkSize), 1.second)

  def getValidators(
    header:                   BlockHeaderValidationAlgebra[F] = mock[BlockHeaderValidationAlgebra[F]],
    headerToBody:             BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]],
    transactionSyntax:        TransactionSyntaxVerifier[F] = mock[TransactionSyntaxVerifier[F]],
    transactionSemantics:     TransactionSemanticValidationAlgebra[F] = mock[TransactionSemanticValidationAlgebra[F]],
    transactionAuthorization: TransactionAuthorizationVerifier[F] = mock[TransactionAuthorizationVerifier[F]],
    bodySyntax:               BodySyntaxValidationAlgebra[F] = mock[BodySyntaxValidationAlgebra[F]],
    bodySemantics:            BodySemanticValidationAlgebra[F] = mock[BodySemanticValidationAlgebra[F]],
    bodyAuthorization:        BodyAuthorizationValidationAlgebra[F] = mock[BodyAuthorizationValidationAlgebra[F]],
    boxState:                 BoxStateAlgebra[F] = mock[BoxStateAlgebra[F]],
    registrationAccumulator:  RegistrationAccumulatorAlgebra[F] = mock[RegistrationAccumulatorAlgebra[F]],
    rewardCalculator:         TransactionRewardCalculatorAlgebra = mock[TransactionRewardCalculatorAlgebra],
    bodyProposalValidationAlgebra: BodyProposalValidationAlgebra[F] = mock[BodyProposalValidationAlgebra[F]]
  ): Validators[F] =
    ValidatorsImpl[F](
      header,
      headerToBody,
      transactionSyntax,
      transactionSemantics,
      transactionAuthorization,
      bodySyntax,
      bodySemantics,
      bodyAuthorization,
      boxState,
      registrationAccumulator,
      rewardCalculator,
      bodyProposalValidationAlgebra
    )

  test("RemoteSlotData: Request no headers if new slot data is worse") {
    PropF.forAllF(arbitraryLinkedSlotDataChainFor(Gen.choose(1, maxChainSize)).arbitrary) {
      (slotData: NonEmptyChain[SlotData]) =>
        withMock {

          val requestsProxy = mock[RequestsProxyActor[F]]

          val localChain = mock[LocalChainAlgebra[F]]
          val currentSlotDataHead = arbitrarySlotData.arbitrary.first
          (() => localChain.head).expects().once().onCall(() => currentSlotDataHead.pure[F])

          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          (slotDataStore.contains).expects(slotData.head.parentSlotId.blockId).returns(true.pure[F])
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val validators = getValidators()
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

          (chainSelectionAlgebra.compare).expects(currentSlotDataHead, slotData.last, *, *).returning(1.pure[F])

          BlockChecker
            .makeActor(
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              validators,
              chainSelectionAlgebra,
              Resource.pure(ed255Vrf),
              defaultConfig
            )
            .use { actor =>
              for {
                _ <- actor.sendNoWait(BlockChecker.Message.RemoteSlotData(hostId, slotData))
              } yield ()
            }
        }
    }
  }

  test("RemoteSlotData: Request headers if new slot data is better and is extension of current chain") {
    PropF.forAllF(arbitraryLinkedSlotDataChainFor(Gen.choose(3, maxChainSize)).arbitrary) {
      (slotData: NonEmptyChain[SlotData]) =>
        withMock {
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val validators = getValidators()
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

          val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))

          // slot data from peer
          val remoteSlotDataAndId = NonEmptyChain.fromChain(idAndSlotData.tail).get
          val (_, remoteSlotData) = remoteSlotDataAndId.unzip

          // local is known data which are stored in stores
          val (localId, localSlotData) = idAndSlotData.head
          (slotDataStore.contains).expects(remoteSlotData.head.parentSlotId.blockId).returns(true.pure[F])
          (headerStore.contains).expects(localId).once().returning(true.pure[F])
          (bodyStore.contains).expects(localId).once().returning(true.pure[F])

          val localSlotDataStore = idAndSlotData.toList.toMap
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              localSlotDataStore(id).pure[F]
            }

          (headerStore.contains).expects(*).anyNumberOfTimes().onCall((id: BlockId) => (id == localId).pure[F])

          val expectedRequest =
            RequestsProxy.Message.DownloadHeadersRequest(
              hostId,
              NonEmptyChain.fromSeq(remoteSlotDataAndId.toChain.toList.take(chunkSize).map(_._1)).get
            ): RequestsProxy.Message
          (requestsProxy.sendNoWait).expects(expectedRequest).once().returning(().pure[F])

          BlockChecker
            .makeActor(
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              validators,
              chainSelectionAlgebra,
              Resource.pure(ed255Vrf),
              defaultConfig,
              Option(BestChain(NonEmptyChain.one(localSlotData)))
            )
            .use { actor =>
              for {
                updatedState <- actor.send(BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData))
                _ = assert(updatedState.bestKnownRemoteSlotDataOpt == Option(BestChain(remoteSlotData)))
              } yield ()
            }
        }
    }
  }

  test("RemoteSlotData: Request headers if new slot data is better and is overlapped of current chain") {
    PropF.forAllF(arbitraryLinkedSlotDataChainFor(Gen.choose(3, maxChainSize)).arbitrary) {
      (slotData: NonEmptyChain[SlotData]) =>
        withMock {
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val validators = getValidators()
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

          val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))

          // slot data from peer
          val remoteSlotDataAndId = NonEmptyChain.fromChain(idAndSlotData.tail).get
          val (_, remoteSlotData) = remoteSlotDataAndId.unzip

          // local is known data which are stored in stores
          val (localId, localSlotData) = idAndSlotData.head
          (slotDataStore.contains).expects(remoteSlotData.head.parentSlotId.blockId).returns(true.pure[F])
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(localId, *, *)
            .once()
            .returning(localSlotData.pure[F])
          (headerStore.contains).expects(localId).once().returning(true.pure[F])
          (bodyStore.contains).expects(localId).once().returning(true.pure[F])

          // known but not accepted yet data
          NonEmptyChain.fromSeq(slotData.toChain.toList.take(2)).get

          val localSlotDataStore = idAndSlotData.toList.toMap
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              localSlotDataStore(id).pure[F]
            }

          (headerStore.contains).expects(*).anyNumberOfTimes().onCall((id: BlockId) => (id == localId).pure[F])

          val expectedRequest =
            RequestsProxy.Message.DownloadHeadersRequest(
              hostId,
              NonEmptyChain.fromSeq(remoteSlotDataAndId.toChain.toList.take(chunkSize).map(_._1)).get
            ): RequestsProxy.Message
          (requestsProxy.sendNoWait).expects(expectedRequest).once().returning(().pure[F])

          BlockChecker
            .makeActor(
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              validators,
              chainSelectionAlgebra,
              Resource.pure(ed255Vrf),
              defaultConfig,
              Option(BestChain(NonEmptyChain.one(remoteSlotData.head)))
            )
            .use { actor =>
              for {
                updatedState <- actor.send(BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData))
                _ = assert(updatedState.bestKnownRemoteSlotDataOpt == Option(BestChain(remoteSlotData)))
              } yield ()
            }
        }
    }
  }

  test("RemoteSlotData: Request headers if new slot data is better and best chain is none") {
    PropF.forAllF(arbitraryLinkedSlotDataChainFor(Gen.choose(3, maxChainSize)).arbitrary) {
      (slotData: NonEmptyChain[SlotData]) =>
        withMock {
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val validators = getValidators()
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

          val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))

          // slot data from peer
          val remoteSlotDataAndId = NonEmptyChain.fromChain(idAndSlotData.tail).get
          val (_, remoteSlotData) = remoteSlotDataAndId.unzip

          // local is known data which are stored in stores
          val (localId, localSlotData) = idAndSlotData.head
          (slotDataStore.contains).expects(remoteSlotData.head.parentSlotId.blockId).returns(true.pure[F])
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(localId, *, *)
            .once()
            .returning(localSlotData.pure[F])

          (() => localChain.head).expects().once().returning(localSlotData.pure[F])

          val localSlotDataStore = idAndSlotData.toList.toMap
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              localSlotDataStore(id).pure[F]
            }

          (headerStore.contains).expects(*).anyNumberOfTimes().onCall((id: BlockId) => (id == localId).pure[F])
          (bodyStore.contains).expects(*).anyNumberOfTimes().onCall((id: BlockId) => (id == localId).pure[F])

          val expectedRequest =
            RequestsProxy.Message.DownloadHeadersRequest(
              hostId,
              NonEmptyChain.fromSeq(remoteSlotDataAndId.toChain.toList.take(chunkSize).map(_._1)).get
            ): RequestsProxy.Message
          (requestsProxy.sendNoWait).expects(expectedRequest).once().returning(().pure[F])

          val expectedBestChain = Option(BestChain(NonEmptyChain.fromChain(slotData.tail).get))

          BlockChecker
            .makeActor(
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              validators,
              chainSelectionAlgebra,
              Resource.pure(ed255Vrf),
              defaultConfig
            )
            .use { actor =>
              for {
                updatedState <- actor.send(BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData))
                _ = assert(updatedState.bestKnownRemoteSlotDataOpt == expectedBestChain)
              } yield ()
            }
        }
    }
  }

  test("RemoteSlotData: Request headers if new slot data is better and current best chain in fork") {
    PropF.forAllF(arbitraryLinkedSlotDataChainFor(Gen.choose(3, maxChainSize)).arbitrary) {
      (slotData: NonEmptyChain[SlotData]) =>
        withMock {
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val validators = getValidators()
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

          val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))

          // slot data from peer
          val remoteSlotDataAndId = NonEmptyChain.fromChain(idAndSlotData.tail).get
          val (_, remoteSlotData) = remoteSlotDataAndId.unzip

          // local is known data which are stored in stores
          val (localId, localSlotData) = idAndSlotData.head
          (slotDataStore.contains).expects(remoteSlotData.head.parentSlotId.blockId).returns(true.pure[F])

          val currentFork =
            arbitraryLinkedSlotDataChainFor(Gen.choose(1L, 1L), localSlotData.some).arbitrary.first
              .prepend(localSlotData)

          // (() => localChain.head).expects().once().returning(localSlotData.pure[F])

          (chainSelectionAlgebra.compare).expects(currentFork.last, slotData.last, *, *).returning((-1).pure[F])

          val localSlotDataStore = idAndSlotData.toList.toMap
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              localSlotDataStore(id).pure[F]
            }

          (headerStore.contains).expects(*).anyNumberOfTimes().onCall((id: BlockId) => (id == localId).pure[F])
          (bodyStore.contains).expects(*).anyNumberOfTimes().onCall((id: BlockId) => (id == localId).pure[F])

          val expectedRequest =
            RequestsProxy.Message.DownloadHeadersRequest(
              hostId,
              NonEmptyChain.fromSeq(remoteSlotDataAndId.toChain.toList.take(chunkSize).map(_._1)).get
            ): RequestsProxy.Message
          (requestsProxy.sendNoWait).expects(expectedRequest).once().returning(().pure[F])

          val bestChain = Option(BestChain(currentFork))
          val expectedBestChain = Option(BestChain(NonEmptyChain.fromChain(slotData.tail).get))

          BlockChecker
            .makeActor(
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              validators,
              chainSelectionAlgebra,
              Resource.pure(ed255Vrf),
              defaultConfig,
              bestChain
            )
            .use { actor =>
              for {
                updatedState <- actor.send(BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData))
                _ = assert(updatedState.bestKnownRemoteSlotDataOpt == expectedBestChain)
              } yield ()
            }
        }
    }
  }

  test("RemoteSlotData: Request headers if new slot data is better, building full chain") {
    PropF.forAllF(arbitraryLinkedSlotDataChainFor(Gen.choose(3, maxChainSize)).arbitrary) {
      (slotData: NonEmptyChain[SlotData]) =>
        withMock {
          mock[PeersManagerActor[F]]
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val validators = getValidators()
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

          val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))

          // local is known data which are stored in stores
          val (localId, _) = idAndSlotData.head
          (headerStore.contains).expects(localId).once().returning(true.pure[F])

          // slot data from peer
          val remoteSize = Gen.choose[Int](2, idAndSlotData.tail.size.toInt - 1).first
          val (missedInChainData, remoteIdAndSlotData) = idAndSlotData.tail.toList.splitAt(remoteSize)
          val (_, peerSlotData) = remoteIdAndSlotData.unzip

          val localSlotDataStore = idAndSlotData.toList.toMap
          (slotDataStore.contains).expects(peerSlotData.head.parentSlotId.blockId).returns(true.pure[F])
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              localSlotDataStore(id).pure[F]
            }

          (headerStore.contains).expects(*).anyNumberOfTimes().onCall((id: BlockId) => (id == localId).pure[F])
          (bodyStore.contains).expects(*).anyNumberOfTimes().onCall((id: BlockId) => (id == localId).pure[F])

          val currentBestChain = NonEmptyChain.fromSeq(slotData.toList.take(2)).get
          (chainSelectionAlgebra.compare).expects(currentBestChain.last, slotData.last, *, *).returning((-1).pure[F])

          val expectedRequest =
            RequestsProxy.Message.DownloadHeadersRequest(
              hostId,
              NonEmptyChain.fromSeq(missedInChainData.take(chunkSize).map(_._1)).get
            ): RequestsProxy.Message
          (requestsProxy.sendNoWait).expects(expectedRequest).once().returning(().pure[F])

          val expectedBestChain = Option(BestChain(NonEmptyChain.fromChain(slotData.tail).get))

          BlockChecker
            .makeActor(
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              validators,
              chainSelectionAlgebra,
              Resource.pure(ed255Vrf),
              defaultConfig,
              Option(BestChain(currentBestChain))
            )
            .use { actor =>
              for {
                updatedState <- actor.send(
                  BlockChecker.Message.RemoteSlotData(hostId, NonEmptyChain.fromSeq(peerSlotData).get)
                )
                _ = assert(updatedState.bestKnownRemoteSlotDataOpt == expectedBestChain)
              } yield ()
            }
        }
    }
  }

  test("RemoteBlockHeader: Do not verify already known headers, do not search unknown body") {
    PropF.forAllF(arbitraryLinkedBlockHeaderChain(Gen.choose(maxChainSize, maxChainSize)).arbitrary) {
      (headers: NonEmptyChain[BlockHeader]) =>
        withMock {
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val validators = getValidators()
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

          (headerStore.contains).expects(*).rep(headers.size.toInt).returning(true.pure[F])
          (validators.header.validate).expects(*).never()
          val slotData = headers.map(h => h.id -> h.slotData).toList.toMap
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              slotData(id).pure[F]
            }

          (bodyStore.contains).expects(*).never().onCall { (id: BlockId) =>
            if (id == slotData.head._2.parentSlotId.blockId) true.pure[F] else false.pure[F]
          }

          val message = headers.map(UnverifiedBlockHeader(hostId, _, 0))
          BlockChecker
            .makeActor(
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              validators,
              chainSelectionAlgebra,
              Resource.pure(ed255Vrf),
              defaultConfig
            )
            .use { actor =>
              for {
                _ <- actor.sendNoWait(BlockChecker.Message.RemoteBlockHeaders(message))
              } yield ()
            }
        }
    }
  }

  test("RemoteBlockHeader: Do not verify already known headers, stop processing if all headers are known") {
    PropF.forAllF(arbitraryLinkedBlockHeaderChain(Gen.choose(2, maxChainSize)).arbitrary) {
      (headers: NonEmptyChain[BlockHeader]) =>
        withMock {
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val validators = getValidators()
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

          (headerStore.contains).expects(*).rep(headers.size.toInt).returning(true.pure[F])

          val message = headers.map(UnverifiedBlockHeader(hostId, _, 0))

          BlockChecker
            .makeActor(
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              validators,
              chainSelectionAlgebra,
              Resource.pure(ed255Vrf),
              defaultConfig
            )
            .use { actor =>
              for {
                _ <- actor.sendNoWait(BlockChecker.Message.RemoteBlockHeaders(message))
              } yield ()
            }
        }
    }
  }

  test("RemoteBlockHeader: Do not verify already known headers, request bodies, request next headers") {
    PropF.forAllF(arbitraryLinkedBlockHeaderChain(Gen.choose(2, maxChainSize)).arbitrary) {
      (headers: NonEmptyChain[BlockHeader]) =>
        withMock {
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val validators = getValidators()
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

          val idAndHeaders = headers.map(h => (h.id, h))

          val knownHeadersSize = Gen.choose[Int](1, headers.size.toInt - 1).first
          val (knownIdAndHeaders, newIdAndHeaders) = idAndHeaders.toList.splitAt(knownHeadersSize)
          val knownSlotData = knownIdAndHeaders.head._2.slotData

          val headerStoreData = mutable.Map.empty[BlockId, BlockHeader] ++ knownIdAndHeaders.toMap
          (headerStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
            headerStoreData.contains(id).pure[F]
          }
          (headerStore.get).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
            headerStoreData.get(id).pure[F]
          }
          (headerStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              headerStoreData(id).pure[F]
            }
          val addedHeader = mutable.Map.empty[BlockId, BlockHeader]
          (headerStore.put).expects(*, *).rep(newIdAndHeaders.size).onCall { case (id: BlockId, header: BlockHeader) =>
            addedHeader.put(id, header)
            headerStoreData.put(id, header)
            ().pure[F]
          }

          (() => localChain.head).stubs().returning(knownSlotData.pure[F])
          ((validators.header.couldBeValidated)).stubs(*, *).returning(true.pure[F])
          ((validators.header.validate))
            .expects(*)
            .rep(newIdAndHeaders.size)
            .onCall((header: BlockHeader) => Either.right[BlockHeaderValidationFailure, BlockHeader](header).pure[F])

          val slotData = headers.map(h => h.id -> h.slotData).toList.toMap
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              slotData(id).pure[F]
            }
          val bodyStoreData = idAndHeaders.toList.toMap
          (bodyStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
            (!bodyStoreData.contains(id)).pure[F]
          }

          val expectedHeaders = NonEmptyChain.fromSeq(idAndHeaders.toList.take(chunkSize).map(_._2)).get
          val expectedMessage: RequestsProxy.Message =
            RequestsProxy.Message.DownloadBodiesRequest(hostId, expectedHeaders)
          (requestsProxy.sendNoWait).expects(expectedMessage).returning(().pure[F])

          val message = headers.map(UnverifiedBlockHeader(hostId, _, 0))

          BlockChecker
            .makeActor(
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              validators,
              chainSelectionAlgebra,
              Resource.pure(ed255Vrf),
              defaultConfig
            )
            .use { actor =>
              for {
                _ <- actor.send(BlockChecker.Message.RemoteBlockHeaders(message))
                _ = assert(newIdAndHeaders.map(_._1).forall(k => addedHeader.contains(k)))
              } yield ()
            }
        }
    }
  }

  test("RemoteBlockHeader: Do not verify non-verifiable headers, do not request next bodies") {
    PropF.forAllF(arbitraryLinkedBlockHeaderChain(Gen.choose(2, maxChainSize)).arbitrary) {
      (headers: NonEmptyChain[BlockHeader]) =>
        withMock {
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val validators = getValidators()
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

          val idAndHeaders = headers.map(h => (h.id, h))

          val knownHeadersSize = Gen.choose[Int](1, headers.size.toInt - 1).first
          val (knownIdAndHeaders, _) = idAndHeaders.toList.splitAt(knownHeadersSize)
          val knownSlotData = knownIdAndHeaders.head._2.slotData(Ed25519VRF.precomputed())

          (headerStore.contains).expects(*).anyNumberOfTimes().returning(false.pure[F])

          (() => localChain.head).stubs().returning(knownSlotData.pure[F])
          ((validators.header.couldBeValidated)).stubs(*, *).returning(false.pure[F])
          val bestChainForKnownAndNewIds: NonEmptyChain[SlotData] =
            idAndHeaders.map { case (id, header) =>
              val parentId = header.parentHeaderId
              val slotId = SlotId(blockId = id)
              val parentSlotId = SlotId(blockId = parentId)
              arbitrarySlotData.arbitrary.first.copy(slotId = slotId, parentSlotId = parentSlotId)
            }
          val newSlotData: Chain[SlotData] = {
            val arSlot = arbitraryLinkedSlotDataChain.arbitrary.retryUntil(c => c.size > 1 && c.size < 10).first
            arSlot.head.copy(parentSlotId = bestChainForKnownAndNewIds.last.slotId) +: arSlot.tail
          }
          val slotData = headers.map(h => h.id -> h.slotData).toList.toMap
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              slotData(id).pure[F]
            }
          val bodyStoreData = idAndHeaders.toList.toMap
          (bodyStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
            (!bodyStoreData.contains(id)).pure[F]
          }

          val bestChain = bestChainForKnownAndNewIds.appendChain(newSlotData)

          val message = headers.map(UnverifiedBlockHeader(hostId, _, 0))

          BlockChecker
            .makeActor(
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              validators,
              chainSelectionAlgebra,
              Resource.pure(ed255Vrf),
              defaultConfig,
              Option(BestChain(bestChain)),
              Option(hostId)
            )
            .use { actor =>
              for {
                res <- actor.send(BlockChecker.Message.RemoteBlockHeaders(message))
                _ = assert(res.bestKnownRemoteSlotDataOpt.get == BestChain(bestChain))
              } yield ()
            }
        }
    }
  }

  test("RemoteBlockHeader: Do not verify already known headers, apply only first header, second is not correct") {
    PropF.forAllF(arbitraryLinkedBlockHeaderChain(Gen.choose(3, maxChainSize)).arbitrary) {
      (headers: NonEmptyChain[BlockHeader]) =>
        withMock {
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val validators = getValidators()
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

          val idAndHeaders = headers.map(h => (h.id, h))

          val knownHeadersSize = Gen.choose[Int](1, headers.size.toInt - 2).first
          val (knownIdAndHeaders, newIdAndHeaders) = idAndHeaders.toList.splitAt(knownHeadersSize)
          val knownSlotData = knownIdAndHeaders.head._2.slotData(Ed25519VRF.precomputed())

          val headerStoreData = mutable.Map.empty[BlockId, BlockHeader] ++ knownIdAndHeaders.toMap
          (headerStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
            headerStoreData.contains(id).pure[F]
          }
          (headerStore.get).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
            headerStoreData.get(id).pure[F]
          }
          (headerStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              headerStoreData(id).pure[F]
            }
          val addedHeader = mutable.Map.empty[BlockId, BlockHeader]
          (headerStore.put).expects(*, *).rep(1).onCall { case (id: BlockId, header: BlockHeader) =>
            addedHeader.put(id, header)
            headerStoreData.put(id, header)
            ().pure[F]
          }

          (() => localChain.head).stubs().returning(knownSlotData.pure[F])
          ((validators.header.couldBeValidated)).stubs(*, *).returning(true.pure[F])
          ((validators.header.validate))
            .expects(*)
            .once()
            .onCall((header: BlockHeader) => Either.right[BlockHeaderValidationFailure, BlockHeader](header).pure[F])
          ((validators.header.validate))
            .expects(*)
            .once()
            .onCall((_: BlockHeader) =>
              Either.left[BlockHeaderValidationFailure, BlockHeader](NonForwardSlot(0, 1)).pure[F]
            )

          val slotData = headers.map(h => h.id -> h.slotData).toList.toMap
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              slotData(id).pure[F]
            }
          val bodyStoreData = idAndHeaders.toList.toMap
          (bodyStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
            (!bodyStoreData.contains(id)).pure[F]
          }

          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              val header = headerStoreData(id)
              val arbSlotData = arbitrarySlotData.arbitrary.first
              val slotData =
                arbSlotData.copy(
                  slotId = arbSlotData.slotId.copy(blockId = header.id),
                  parentSlotId = arbSlotData.parentSlotId.copy(blockId = header.parentHeaderId)
                )
              slotData.pure[F]
            }

          val expectedHeaders = NonEmptyChain.fromSeq(idAndHeaders.toList.take(chunkSize).map(_._2)).get
          val expectedMessage: RequestsProxy.Message =
            RequestsProxy.Message.DownloadBodiesRequest(hostId, expectedHeaders)
          (requestsProxy.sendNoWait)
            .expects(expectedMessage)
            .returning(().pure[F])
          (requestsProxy.sendNoWait)
            .expects(RequestsProxy.Message.InvalidateBlockId(hostId, newIdAndHeaders(1)._1))
            .returning(().pure[F])
          (requestsProxy.sendNoWait)
            .expects(RequestsProxy.Message.ResetRequestsProxy)
            .returns(().pure[F])

          val message = headers.map(UnverifiedBlockHeader(hostId, _, 0))

          BlockChecker
            .makeActor(
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              validators,
              chainSelectionAlgebra,
              Resource.pure(ed255Vrf),
              defaultConfig,
              bestChain = Option(BestChain(NonEmptyChain.fromSeq(newIdAndHeaders.map(d => headerToSlotData(d._2))).get))
            )
            .use { actor =>
              for {
                state <- actor.send(BlockChecker.Message.RemoteBlockHeaders(message))
                _ = assert(addedHeader.contains(newIdAndHeaders.head._1))
                _ = assert(state.bestKnownRemoteSlotDataOpt.isEmpty)
                _ = assert(state.bestKnownRemoteSlotDataHost.isEmpty)
              } yield ()
            }
        }
    }
  }

  test(
    "RemoteBlockHeader: Do not verify already known headers, apply only first header, exception during processing second"
  ) {
    PropF.forAllF(arbitraryLinkedBlockHeaderChain(Gen.choose(3, maxChainSize)).arbitrary) {
      (headers: NonEmptyChain[BlockHeader]) =>
        withMock {
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val validators = getValidators()
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

          val idAndHeaders = headers.map(h => (h.id, h))

          val knownHeadersSize = Gen.choose[Int](1, headers.size.toInt - 2).first
          val (knownIdAndHeaders, newIdAndHeaders) = idAndHeaders.toList.splitAt(knownHeadersSize)
          val knownSlotData = knownIdAndHeaders.head._2.slotData(Ed25519VRF.precomputed())

          val headerStoreData = mutable.Map.empty[BlockId, BlockHeader] ++ knownIdAndHeaders.toMap
          (headerStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
            headerStoreData.contains(id).pure[F]
          }
          (headerStore.get).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
            headerStoreData.get(id).pure[F]
          }
          (headerStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              headerStoreData(id).pure[F]
            }
          val addedHeader = mutable.Map.empty[BlockId, BlockHeader]
          (headerStore.put).expects(*, *).rep(1).onCall { case (id: BlockId, header: BlockHeader) =>
            addedHeader.put(id, header)
            headerStoreData.put(id, header)
            ().pure[F]
          }

          (() => localChain.head).stubs().returning(knownSlotData.pure[F])
          (validators.header.couldBeValidated).stubs(*, *).returning(true.pure[F])
          (validators.header.validate)
            .expects(*)
            .once()
            .onCall((header: BlockHeader) => Either.right[BlockHeaderValidationFailure, BlockHeader](header).pure[F])
          ((validators.header.validate))
            .expects(*)
            .once()
            .onCall((id: BlockHeader) => throw new IllegalStateException(show"Error for id $id"))

          val slotData = headers.map(h => h.id -> h.slotData).toList.toMap
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              slotData(id).pure[F]
            }
          val bodyStoreData = idAndHeaders.toList.toMap
          (bodyStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
            (!bodyStoreData.contains(id)).pure[F]
          }

          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              val header = headerStoreData(id)
              val arbSlotData = arbitrarySlotData.arbitrary.first
              val slotData =
                arbSlotData.copy(
                  slotId = arbSlotData.slotId.copy(blockId = header.id),
                  parentSlotId = arbSlotData.parentSlotId.copy(blockId = header.parentHeaderId)
                )
              slotData.pure[F]
            }

          val expectedHeaders = NonEmptyChain.fromSeq(idAndHeaders.toList.take(chunkSize).map(_._2)).get
          val expectedMessage: RequestsProxy.Message =
            RequestsProxy.Message.DownloadBodiesRequest(hostId, expectedHeaders)
          (requestsProxy.sendNoWait)
            .expects(expectedMessage)
            .returning(().pure[F])

          val message = headers.map(UnverifiedBlockHeader(hostId, _, 0))

          BlockChecker
            .makeActor(
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              validators,
              chainSelectionAlgebra,
              Resource.pure(ed255Vrf),
              defaultConfig,
              bestChain = Option(BestChain(NonEmptyChain.fromSeq(newIdAndHeaders.map(d => headerToSlotData(d._2))).get))
            )
            .use { actor =>
              for {
                _ <- actor.send(BlockChecker.Message.RemoteBlockHeaders(message))
                _ = assert(addedHeader.contains(newIdAndHeaders.head._1))
              } yield ()
            }
        }
    }
  }

  test("RemoteBlockBodies: Skip verification already known bodies") {
    PropF.forAllF(
      nonEmptyChainArbOfLen(ModelGenerators.arbitraryNodeBody, Gen.choose(2, maxChainSize).first).arbitrary
    ) { (bodies: NonEmptyChain[BlockBody]) =>
      withMock {
        val requestsProxy = mock[RequestsProxyActor[F]]
        val localChain = mock[LocalChainAlgebra[F]]
        val slotDataStore = mock[Store[F, BlockId, SlotData]]
        val headerStore = mock[Store[F, BlockId, BlockHeader]]
        val bodyStore = mock[Store[F, BlockId, BlockBody]]
        val validators = getValidators()
        val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

        val ids: NonEmptyChain[BlockHeader] = bodies.map(_ => arbitraryHeader.arbitrary.first)

        val blocks: NonEmptyChain[Block] =
          ids.zipWith(bodies)((_, _)).map { case (header, body) => Block(header, body) }

        val message =
          BlockChecker.Message.RemoteBlockBodies(blocks.map(d => (d.header, UnverifiedBlockBody(hostId, d.body, 0))))

        (bodyStore.contains).expects(*).rep(bodies.size.toInt).returning(true.pure[F])

        BlockChecker
          .makeActor(
            requestsProxy,
            localChain,
            slotDataStore,
            headerStore,
            bodyStore,
            validators,
            chainSelectionAlgebra,
            Resource.pure(ed255Vrf),
            defaultConfig
          )
          .use { actor =>
            for {
              _ <- actor.send(message)
            } yield ()
          }
      }
    }
  }

  test("RemoteBlockBodies: Verify and save new blocks, but not apply to local chain because it worse") {
    PropF.forAllF(
      nonEmptyChainArbOfLen(ModelGenerators.arbitraryNodeBody, Gen.choose(2, maxChainSize).first).arbitrary
    ) { (bodies: NonEmptyChain[BlockBody]) =>
      withMock {
        val requestsProxy = mock[RequestsProxyActor[F]]
        val localChain = mock[LocalChainAlgebra[F]]
        val slotDataStore = mock[Store[F, BlockId, SlotData]]
        val headerStore = mock[Store[F, BlockId, BlockHeader]]
        val bodyStore = mock[Store[F, BlockId, BlockBody]]
        val validators = getValidators()
        val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

        val headers: NonEmptyChain[BlockHeader] = bodies.map(_ => arbitraryHeader.arbitrary.first).map(_.embedId)

        val blocks: NonEmptyChain[Block] =
          headers.zipWith(bodies)((_, _)).map { case (header, body) => Block(header, body) }
        val idAndHeader: NonEmptyChain[(BlockId, BlockHeader)] = headers.map(h => (h.id, h))
        val idAndBody = blocks.map(block => (block.header.id, block.body))

        val message =
          BlockChecker.Message.RemoteBlockBodies(blocks.map(d => (d.header, UnverifiedBlockBody(hostId, d.body, 0))))

        val knownBodiesSize = Gen.choose[Int](1, bodies.size.toInt - 1).first
        val (knownIdAndHeaders, newIdAndHeaders) = idAndBody.toList.splitAt(knownBodiesSize)

        val newBodiesSize = newIdAndHeaders.size

        val knownBodyStorageData = knownIdAndHeaders.toMap
        (bodyStore.contains).expects(*).rep(bodies.size.toInt).onCall { (id: BlockId) =>
          knownBodyStorageData.contains(id).pure[F]
        }

        val headerStorageData = idAndHeader.toChain.toList.toMap
        (headerStore
          .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
          .expects(*, *, *)
          .anyNumberOfTimes()
          .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
            headerStorageData(id).pure[F]
          }

        (validators.bodySyntax.validate).expects(*).rep(newBodiesSize).onCall { (b: BlockBody) =>
          Validated.validNec[BodySyntaxError, BlockBody](b).pure[F]
        }

        (validators.bodySemantics
          .validate(_: BodyValidationContext)(_: BlockBody))
          .expects(*, *)
          .rep(newBodiesSize)
          .onCall { case (_: BodyValidationContext, b: BlockBody) =>
            Validated.validNec[BodySemanticError, BlockBody](b).pure[F]
          }

        (validators.bodyProposalValidationAlgebra
          .validate(_: BodyProposalValidationContext)(_: BlockBody))
          .expects(*, *)
          .rep(newBodiesSize)
          .onCall { case (_: BodyProposalValidationContext, b: BlockBody) =>
            Validated.validNec[BodySemanticError, BlockBody](b).pure[F]
          }

        type AuthContext = IoTransaction => DynamicContext[F, String, Datum]
        (validators.bodyAuthorization
          .validate(_: AuthContext)(_: BlockBody))
          .expects(*, *)
          .rep(newBodiesSize)
          .onCall { case (_: AuthContext @unchecked, b: BlockBody) =>
            Validated.validNec[BodyAuthorizationError, BlockBody](b).pure[F]
          }

        val storedBodies = mutable.Map.empty[BlockId, BlockBody]
        (bodyStore.put).expects(*, *).rep(newBodiesSize).onCall { case (id: BlockId, block: BlockBody) =>
          storedBodies.put(id, block).pure[F].void
        }

        val slotsStorageData = idAndHeader.map { case (id, header) => (id, headerToSlotData(header)) }.toList.toMap
        (slotDataStore.get).expects(*).anyNumberOfTimes().onCall((id: BlockId) => slotsStorageData.get(id).pure[F])
        (slotDataStore
          .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
          .expects(*, *, *)
          .anyNumberOfTimes()
          .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
            slotsStorageData(id).pure[F]
          }

        (localChain.isWorseThan).expects(*).anyNumberOfTimes().returning(false.pure[F])

        BlockChecker
          .makeActor(
            requestsProxy,
            localChain,
            slotDataStore,
            headerStore,
            bodyStore,
            validators,
            chainSelectionAlgebra,
            Resource.pure(ed255Vrf),
            defaultConfig
          )
          .use { actor =>
            for {
              _ <- actor.send(message)
            } yield ()
          }
      }
    }
  }

  test("RemoteBlockBodies: Verify and save blocks, apply to local chain, send no new request") {
    PropF.forAllF(
      nonEmptyChainArbOfLen(ModelGenerators.arbitraryNodeBody, Gen.choose(2, maxChainSize).first).arbitrary
    ) { (bodies: NonEmptyChain[BlockBody]) =>
      withMock {
        val requestsProxy = mock[RequestsProxyActor[F]]
        val localChain = mock[LocalChainAlgebra[F]]
        val slotDataStore = mock[Store[F, BlockId, SlotData]]
        val headerStore = mock[Store[F, BlockId, BlockHeader]]
        val bodyStore = mock[Store[F, BlockId, BlockBody]]
        val validators = getValidators()
        val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

        val headers: NonEmptyChain[BlockHeader] = bodies.map(_ => arbitraryHeader.arbitrary.first).map(_.embedId)

        val blocks: NonEmptyChain[Block] =
          headers.zipWith(bodies)((_, _)).map { case (header, body) => Block(header, body) }
        val idAndHeader: NonEmptyChain[(BlockId, BlockHeader)] = headers.map(h => (h.id, h))
        val idAndBody = blocks.map(block => (block.header.id, block.body))

        val message =
          BlockChecker.Message.RemoteBlockBodies(blocks.map(d => (d.header, UnverifiedBlockBody(hostId, d.body, 0))))

        val knownBodiesSize = Gen.choose[Int](1, bodies.size.toInt - 1).first
        val (knownIdAndHeaders, newIdAndHeaders) = idAndBody.toList.splitAt(knownBodiesSize)

        val newBodiesSize = newIdAndHeaders.size

        val knownBodyStorageData = knownIdAndHeaders.toMap
        (bodyStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
          knownBodyStorageData.contains(id).pure[F]
        }

        val headerStorageData = idAndHeader.toChain.toList.toMap
        (headerStore
          .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
          .expects(*, *, *)
          .anyNumberOfTimes()
          .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
            headerStorageData(id).pure[F]
          }

        (validators.bodySyntax.validate).expects(*).rep(newBodiesSize).onCall { (b: BlockBody) =>
          Validated.validNec[BodySyntaxError, BlockBody](b).pure[F]
        }

        (validators.bodySemantics
          .validate(_: BodyValidationContext)(_: BlockBody))
          .expects(*, *)
          .rep(newBodiesSize)
          .onCall { case (_: BodyValidationContext, b: BlockBody) =>
            Validated.validNec[BodySemanticError, BlockBody](b).pure[F]
          }

        (validators.bodyProposalValidationAlgebra
          .validate(_: BodyProposalValidationContext)(_: BlockBody))
          .expects(*, *)
          .rep(newBodiesSize)
          .onCall { case (_: BodyProposalValidationContext, b: BlockBody) =>
            Validated.validNec[BodySemanticError, BlockBody](b).pure[F]
          }

        type AuthContext = IoTransaction => DynamicContext[F, String, Datum]
        (validators.bodyAuthorization
          .validate(_: AuthContext)(_: BlockBody))
          .expects(*, *)
          .rep(newBodiesSize)
          .onCall { case (_: AuthContext @unchecked, b: BlockBody) =>
            Validated.validNec[BodyAuthorizationError, BlockBody](b).pure[F]
          }

        val storedBodies = mutable.Map.empty[BlockId, BlockBody]
        (bodyStore.put).expects(*, *).rep(newBodiesSize).onCall { case (id: BlockId, block: BlockBody) =>
          storedBodies.put(id, block).pure[F].void
        }

        val idAndSlotData = idAndHeader.map { case (id, header) => (id, headerToSlotData(header)) }.toList
        val slotsStorageData = idAndSlotData.toMap
        (slotDataStore.get).expects(*).anyNumberOfTimes().onCall((id: BlockId) => slotsStorageData.get(id).pure[F])
        (slotDataStore
          .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
          .expects(*, *, *)
          .anyNumberOfTimes()
          .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
            slotsStorageData(id).pure[F]
          }

        val lastBlockSlotData = idAndSlotData.last._2

        (localChain.isWorseThan).expects(*).anyNumberOfTimes().onCall { (ids: NonEmptyChain[SlotData]) =>
          (lastBlockSlotData === ids.last).pure[F]
        }

        (localChain.adopt).expects(Validated.Valid(lastBlockSlotData)).once().returning(().pure[F])

        (requestsProxy.sendNoWait).expects(*).never()

        BlockChecker
          .makeActor(
            requestsProxy,
            localChain,
            slotDataStore,
            headerStore,
            bodyStore,
            validators,
            chainSelectionAlgebra,
            Resource.pure(ed255Vrf),
            defaultConfig,
            Option(BestChain(NonEmptyChain.one(lastBlockSlotData)))
          )
          .use { actor =>
            for {
              newState <- actor.send(message)
              _ = assert(newState.bestKnownRemoteSlotDataOpt.isEmpty)
            } yield ()
          }
      }
    }
  }

  test("RemoteBlockBodies: Verify and save blocks, apply to local chain, send new request due headers exist") {
    withMock {
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val validators = getValidators()
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

      // [1 - 5) -- known SlotData known Header known Body
      // [5 - 10) -- known SlotData known Header unknown Body
      // [10 - 15) -- known SlotData unknown Header unknown Body
      // Minimum length for allIdSlotDataHeaderBlock is 11 then
      val knownSlotHeaderBodyLen = 5
      val knownSlotHeaderUnknownBodyLen = 10
      val downloadedBodies = Gen.choose(knownSlotHeaderBodyLen + 1, knownSlotHeaderUnknownBodyLen - 1).first
      val knownSlotUnknownHeaderBodyLen = 15
      val allIdSlotDataHeaderBlock =
        arbitraryLinkedSlotDataHeaderBlockNoTx(
          Gen.choose(knownSlotUnknownHeaderBodyLen, knownSlotUnknownHeaderBodyLen)
        ).arbitrary.first.toList

      val knowSlotData =
        allIdSlotDataHeaderBlock.map(data => (data._1, data._2))

      val knownHeaders =
        allIdSlotDataHeaderBlock.take(knownSlotHeaderUnknownBodyLen).map(d => (d._1, d._3))

      val knownBody =
        allIdSlotDataHeaderBlock.take(knownSlotHeaderBodyLen).map(d => (d._1, d._4))

      val requestIdSlotDataHeaderBlock =
        allIdSlotDataHeaderBlock.slice(knownSlotHeaderBodyLen, downloadedBodies)
      val requestIdSlotDataHeaderBlockSize = requestIdSlotDataHeaderBlock.size

      val expectedNewRequest = allIdSlotDataHeaderBlock.drop(downloadedBodies)

      val messageData = NonEmptyChain.fromSeq(requestIdSlotDataHeaderBlock.map(d => Block(d._3, d._4))).get

      val message =
        BlockChecker.Message.RemoteBlockBodies(messageData.map(d => (d.header, UnverifiedBlockBody(hostId, d.body, 0))))

      val knownBodyStorageData: mutable.Map[BlockId, BlockBody] =
        mutable.Map.empty[BlockId, BlockBody] ++ knownBody.toMap
      (bodyStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        knownBodyStorageData.contains(id).pure[F]
      }
      (bodyStore.put).expects(*, *).rep(requestIdSlotDataHeaderBlockSize).onCall {
        case (id: BlockId, block: BlockBody) =>
          knownBodyStorageData.put(id, block).pure[F].void
      }

      val headerStorageData = knownHeaders.toMap
      (headerStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          headerStorageData(id).pure[F]
        }
      (headerStore.get).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        headerStorageData.get(id).pure[F]
      }
      (headerStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        headerStorageData.contains(id).pure[F]
      }

      (validators.bodySyntax.validate).expects(*).rep(requestIdSlotDataHeaderBlockSize).onCall { (b: BlockBody) =>
        Validated.validNec[BodySyntaxError, BlockBody](b).pure[F]
      }

      (validators.bodySemantics
        .validate(_: BodyValidationContext)(_: BlockBody))
        .expects(*, *)
        .rep(requestIdSlotDataHeaderBlockSize)
        .onCall { case (_: BodyValidationContext, b: BlockBody) =>
          Validated.validNec[BodySemanticError, BlockBody](b).pure[F]
        }

      (validators.bodyProposalValidationAlgebra
        .validate(_: BodyProposalValidationContext)(_: BlockBody))
        .expects(*, *)
        .rep(requestIdSlotDataHeaderBlockSize)
        .onCall { case (_: BodyProposalValidationContext, b: BlockBody) =>
          Validated.validNec[BodySemanticError, BlockBody](b).pure[F]
        }

      type AuthContext = IoTransaction => DynamicContext[F, String, Datum]
      (validators.bodyAuthorization
        .validate(_: AuthContext)(_: BlockBody))
        .expects(*, *)
        .rep(requestIdSlotDataHeaderBlockSize)
        .onCall { case (_: AuthContext @unchecked, b: BlockBody) =>
          Validated.validNec[BodyAuthorizationError, BlockBody](b).pure[F]
        }

      val slotDataStoreData = knowSlotData.toMap
      (slotDataStore.get).expects(*).anyNumberOfTimes().onCall((id: BlockId) => slotDataStoreData.get(id).pure[F])
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreData(id).pure[F]
        }

      (localChain.isWorseThan).expects(*).rep(requestIdSlotDataHeaderBlockSize).returning(true.pure[F])
      (localChain.adopt).expects(*).rep(requestIdSlotDataHeaderBlockSize).returning(().pure[F])

      val expectedNewRequestMessage: RequestsProxy.Message =
        RequestsProxy.Message.DownloadBodiesRequest(
          hostId,
          NonEmptyChain.fromSeq(expectedNewRequest.map(d => d._3.embedId).take(chunkSize)).get
        )
      (requestsProxy.sendNoWait).expects(expectedNewRequestMessage).once().returning(().pure[F])

      BlockChecker
        .makeActor(
          requestsProxy,
          localChain,
          slotDataStore,
          headerStore,
          bodyStore,
          validators,
          chainSelectionAlgebra,
          Resource.pure(ed255Vrf),
          defaultConfig,
          Option(BestChain(NonEmptyChain.one(allIdSlotDataHeaderBlock.last._2))),
          Option(hostId)
        )
        .use { actor =>
          for {
            newState <- actor.send(message)
            _ = assert(
              newState.bestKnownRemoteSlotDataOpt == Option(
                BestChain(NonEmptyChain.one(allIdSlotDataHeaderBlock.last._2))
              )
            )
          } yield ()
        }
    }
  }

  test("RemoteBlockBodies: Verify and save blocks, apply to local chain, send new request due headers") {
    withMock {
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val validators = getValidators()
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

      // [1 - 5) -- known SlotData known Header known Body
      // [5 - 10) -- known SlotData known Header unknown Body
      // [10 - 15) -- known SlotData unknown Header unknown Body
      // Minimum length for allIdSlotDataHeaderBlock is 11 then
      val knownSlotHeaderBodyLen = 5
      val knownSlotHeaderUnknownBodyLen = 10
      val downloadedBodies = 10
      val knownSlotUnknownHeaderBodyLen = 15
      val allIdSlotDataHeaderBlock =
        arbitraryLinkedSlotDataHeaderBlockNoTx(
          Gen.choose(knownSlotUnknownHeaderBodyLen, knownSlotUnknownHeaderBodyLen)
        ).arbitrary.first.toList

      val knowSlotData =
        allIdSlotDataHeaderBlock.map(data => (data._1, data._2))

      val knownHeaders =
        allIdSlotDataHeaderBlock.take(knownSlotHeaderUnknownBodyLen).map(d => (d._1, d._3))

      val knownBody =
        allIdSlotDataHeaderBlock.take(knownSlotHeaderBodyLen).map(d => (d._1, d._4))

      val requestIdSlotDataHeaderBlock =
        allIdSlotDataHeaderBlock.slice(knownSlotHeaderBodyLen, downloadedBodies)
      val requestIdSlotDataHeaderBlockSize = requestIdSlotDataHeaderBlock.size

      val messageData =
        NonEmptyChain.fromSeq(requestIdSlotDataHeaderBlock.map(d => Block(d._3, d._4))).get

      val message =
        BlockChecker.Message.RemoteBlockBodies(messageData.map(d => (d.header, UnverifiedBlockBody(hostId, d.body, 0))))

      val knownBodyStorageData: mutable.Map[BlockId, BlockBody] =
        mutable.Map.empty[BlockId, BlockBody] ++ knownBody.toMap
      (bodyStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        knownBodyStorageData.contains(id).pure[F]
      }
      (bodyStore.put).expects(*, *).rep(requestIdSlotDataHeaderBlockSize).onCall {
        case (id: BlockId, block: BlockBody) =>
          knownBodyStorageData.put(id, block).pure[F].void
      }

      val headerStorageData = knownHeaders.toMap
      (headerStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          headerStorageData(id).pure[F]
        }
      (headerStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        headerStorageData.contains(id).pure[F]
      }
      (headerStore.get).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        headerStorageData.get(id).pure[F]
      }

      (validators.bodySyntax.validate).expects(*).rep(requestIdSlotDataHeaderBlockSize).onCall { (b: BlockBody) =>
        Validated.validNec[BodySyntaxError, BlockBody](b).pure[F]
      }

      (validators.bodySemantics
        .validate(_: BodyValidationContext)(_: BlockBody))
        .expects(*, *)
        .rep(requestIdSlotDataHeaderBlockSize)
        .onCall { case (_: BodyValidationContext, b: BlockBody) =>
          Validated.validNec[BodySemanticError, BlockBody](b).pure[F]
        }

      (validators.bodyProposalValidationAlgebra
        .validate(_: BodyProposalValidationContext)(_: BlockBody))
        .expects(*, *)
        .rep(requestIdSlotDataHeaderBlockSize)
        .onCall { case (_: BodyProposalValidationContext, b: BlockBody) =>
          Validated.validNec[BodySemanticError, BlockBody](b).pure[F]
        }

      type AuthContext = IoTransaction => DynamicContext[F, String, Datum]
      (validators.bodyAuthorization
        .validate(_: AuthContext)(_: BlockBody))
        .expects(*, *)
        .rep(requestIdSlotDataHeaderBlockSize)
        .onCall { case (_: AuthContext @unchecked, b: BlockBody) =>
          Validated.validNec[BodyAuthorizationError, BlockBody](b).pure[F]
        }

      val slotDataStoreData = knowSlotData.toMap
      (slotDataStore.get).expects(*).anyNumberOfTimes().onCall((id: BlockId) => slotDataStoreData.get(id).pure[F])
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreData(id).pure[F]
        }

      (localChain.isWorseThan).expects(*).rep(requestIdSlotDataHeaderBlockSize).returning(true.pure[F])
      (localChain.adopt).expects(*).rep(requestIdSlotDataHeaderBlockSize).returning(().pure[F])

      val headersRequest =
        allIdSlotDataHeaderBlock.slice(knownSlotHeaderUnknownBodyLen, knownSlotHeaderUnknownBodyLen + 1).map(_._1)
      val headerReqMessage =
        RequestsProxy.Message.DownloadHeadersRequest(hostId, NonEmptyChain.fromSeq(headersRequest).get)
      (requestsProxy.sendNoWait).expects(headerReqMessage).returning(().pure[F])

      BlockChecker
        .makeActor(
          requestsProxy,
          localChain,
          slotDataStore,
          headerStore,
          bodyStore,
          validators,
          chainSelectionAlgebra,
          Resource.pure(ed255Vrf),
          defaultConfig,
          Option(BestChain(NonEmptyChain.one(allIdSlotDataHeaderBlock.last._2))),
          Option(hostId)
        )
        .use { actor =>
          for {
            newState <- actor.send(message)
            _ = assert(
              newState.bestKnownRemoteSlotDataOpt == Option(
                BestChain(NonEmptyChain.one(allIdSlotDataHeaderBlock.last._2))
              )
            )
          } yield ()
        }
    }
  }

  test("RemoteBlockBodies: Verify and save only first block because of error in validation") {
    withMock {
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val validators = getValidators()
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

      // [1 - 5) -- known SlotData known Header known Body
      // [5 - 10) -- known SlotData known Header unknown Body
      // [10 - 15) -- known SlotData unknown Header unknown Body
      // Minimum length for allIdSlotDataHeaderBlock is 11 then
      val knownSlotHeaderBodyLen = 5
      val knownSlotHeaderUnknownBodyLen = 10
      val downloadedBodies = 10
      val knownSlotUnknownHeaderBodyLen = 15
      val allIdSlotDataHeaderBlock =
        arbitraryLinkedSlotDataHeaderBlockNoTx(
          Gen.choose(knownSlotUnknownHeaderBodyLen, knownSlotUnknownHeaderBodyLen)
        ).arbitrary.first.toList

      val knowSlotData =
        allIdSlotDataHeaderBlock.map(data => (data._1, data._2))

      val knownHeaders =
        allIdSlotDataHeaderBlock.take(knownSlotHeaderUnknownBodyLen).map(d => (d._1, d._3))

      val knownBody =
        allIdSlotDataHeaderBlock.take(knownSlotHeaderBodyLen).map(d => (d._1, d._4))

      val requestIdSlotDataHeaderBlock =
        allIdSlotDataHeaderBlock.slice(knownSlotHeaderBodyLen, downloadedBodies)

      val messageData =
        NonEmptyChain.fromSeq(requestIdSlotDataHeaderBlock.map(d => Block(d._3, d._4))).get

      val message =
        BlockChecker.Message.RemoteBlockBodies(messageData.map(d => (d.header, UnverifiedBlockBody(hostId, d.body, 0))))

      val knownBodyStorageData: mutable.Map[BlockId, BlockBody] =
        mutable.Map.empty[BlockId, BlockBody] ++ knownBody.toMap
      (bodyStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        knownBodyStorageData.contains(id).pure[F]
      }
      (bodyStore.put(_: BlockId, _: BlockBody)).expects(*, *).anyNumberOfTimes().onCall {
        case (id: BlockId, block: BlockBody) =>
          knownBodyStorageData.put(id, block).pure[F].void
      }

      val headerStorageData = knownHeaders.toMap
      (headerStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          headerStorageData(id).pure[F]
        }
      (headerStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        headerStorageData.contains(id).pure[F]
      }
      (headerStore.get).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        headerStorageData.get(id).pure[F]
      }

      (validators.bodySyntax.validate).expects(*).rep(2).onCall { (b: BlockBody) =>
        Validated.validNec[BodySyntaxError, BlockBody](b).pure[F]
      }

      val errorTransaction = arbitraryIoTransaction.arbitrary.first
      val spentTransactionOutput = arbitrarySpentTransactionOutput.arbitrary.first
      (validators.bodySemantics
        .validate(_: BodyValidationContext)(_: BlockBody))
        .expects(*, *)
        .rep(2)
        .onCall { case (context: BodyValidationContext, b: BlockBody) =>
          val secondBlock: Boolean = context.parentHeaderId != requestIdSlotDataHeaderBlock.head._1
          val error =
            TransactionSemanticErrors(errorTransaction, NonEmptyChain.one(InputDataMismatch(spentTransactionOutput)))
          Validated.condNec[BodySemanticError, BlockBody](secondBlock, b, error).pure[F]
        }

      (validators.bodyProposalValidationAlgebra
        .validate(_: BodyProposalValidationContext)(_: BlockBody))
        .expects(*, *)
        .once()
        .onCall { case (_: BodyProposalValidationContext, b: BlockBody) =>
          Validated.validNec[BodySemanticError, BlockBody](b).pure[F]
        }

      type AuthContext = IoTransaction => DynamicContext[F, String, Datum]
      (validators.bodyAuthorization
        .validate(_: AuthContext)(_: BlockBody))
        .expects(*, *)
        .rep(1)
        .onCall { case (_: AuthContext @unchecked, b: BlockBody) =>
          Validated.validNec[BodyAuthorizationError, BlockBody](b).pure[F]
        }

      val slotDataStoreData = knowSlotData.toMap
      (slotDataStore.get).expects(*).anyNumberOfTimes().onCall((id: BlockId) => slotDataStoreData.get(id).pure[F])
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreData(id).pure[F]
        }

      val lastAdoptedBlockSlotData = requestIdSlotDataHeaderBlock.head._2
      (localChain.isWorseThan).expects(*).anyNumberOfTimes().onCall { (ids: NonEmptyChain[SlotData]) =>
        assert(lastAdoptedBlockSlotData == ids.last)
        (lastAdoptedBlockSlotData == ids.last).pure[F]
      }
      (localChain.adopt).expects(Validated.Valid(lastAdoptedBlockSlotData)).once().returning(().pure[F])

      (requestsProxy.sendNoWait)
        .expects(RequestsProxy.Message.InvalidateBlockId(hostId, requestIdSlotDataHeaderBlock(1)._1))
        .returns(().pure[F])
      (requestsProxy.sendNoWait)
        .expects(RequestsProxy.Message.ResetRequestsProxy)
        .returns(().pure[F])

      BlockChecker
        .makeActor(
          requestsProxy,
          localChain,
          slotDataStore,
          headerStore,
          bodyStore,
          validators,
          chainSelectionAlgebra,
          Resource.pure(ed255Vrf),
          defaultConfig,
          Option(BestChain(NonEmptyChain.fromSeq(allIdSlotDataHeaderBlock.map(_._2)).get)),
          Option(hostId)
        )
        .use { actor =>
          for {
            _ <- actor.send(message)
          } yield ()
        }
    }
  }

  test("RemoteBlockBodies: Verify and save only first block because of unknown error in validation") {
    withMock {
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val validators = getValidators()
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

      // [1 - 5) -- known SlotData known Header known Body
      // [5 - 10) -- known SlotData known Header unknown Body
      // [10 - 15) -- known SlotData unknown Header unknown Body
      // Minimum length for allIdSlotDataHeaderBlock is 11 then
      val knownSlotHeaderBodyLen = 5
      val knownSlotHeaderUnknownBodyLen = 10
      val downloadedBodies = 10
      val knownSlotUnknownHeaderBodyLen = 15
      val allIdSlotDataHeaderBlock =
        arbitraryLinkedSlotDataHeaderBlockNoTx(
          Gen.choose(knownSlotUnknownHeaderBodyLen, knownSlotUnknownHeaderBodyLen)
        ).arbitrary.first.toList

      val knowSlotData =
        allIdSlotDataHeaderBlock.map(data => (data._1, data._2))

      val knownHeaders =
        allIdSlotDataHeaderBlock.take(knownSlotHeaderUnknownBodyLen).map(d => (d._1, d._3))

      val knownBody =
        allIdSlotDataHeaderBlock.take(knownSlotHeaderBodyLen).map(d => (d._1, d._4))

      val requestIdSlotDataHeaderBlock =
        allIdSlotDataHeaderBlock.slice(knownSlotHeaderBodyLen, downloadedBodies)

      val messageData =
        NonEmptyChain.fromSeq(requestIdSlotDataHeaderBlock.map(d => Block(d._3, d._4))).get

      val message =
        BlockChecker.Message.RemoteBlockBodies(messageData.map(d => (d.header, UnverifiedBlockBody(hostId, d.body, 0))))

      val knownBodyStorageData: mutable.Map[BlockId, BlockBody] =
        mutable.Map.empty[BlockId, BlockBody] ++ knownBody.toMap
      (bodyStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        knownBodyStorageData.contains(id).pure[F]
      }
      (bodyStore.put(_: BlockId, _: BlockBody)).expects(*, *).anyNumberOfTimes().onCall {
        case (id: BlockId, block: BlockBody) =>
          knownBodyStorageData.put(id, block).pure[F].void
      }

      val headerStorageData = knownHeaders.toMap
      (headerStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          headerStorageData(id).pure[F]
        }
      (headerStore.contains).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        headerStorageData.contains(id).pure[F]
      }
      (headerStore.get).expects(*).anyNumberOfTimes().onCall { (id: BlockId) =>
        headerStorageData.get(id).pure[F]
      }

      (validators.bodySyntax.validate).expects(*).once().onCall { (b: BlockBody) =>
        Validated.validNec[BodySyntaxError, BlockBody](b).pure[F]
      }

      (validators.bodySyntax.validate).expects(*).once().onCall { (_: BlockBody) =>
        throw new IllegalStateException()
      }

      val errorTransaction = arbitraryIoTransaction.arbitrary.first
      val spentTransactionOutput = arbitrarySpentTransactionOutput.arbitrary.first
      (validators.bodySemantics
        .validate(_: BodyValidationContext)(_: BlockBody))
        .expects(*, *)
        .once()
        .onCall { case (context: BodyValidationContext, b: BlockBody) =>
          val secondBlock: Boolean = context.parentHeaderId != requestIdSlotDataHeaderBlock.head._1
          val error =
            TransactionSemanticErrors(errorTransaction, NonEmptyChain.one(InputDataMismatch(spentTransactionOutput)))
          Validated.condNec[BodySemanticError, BlockBody](secondBlock, b, error).pure[F]
        }

      (validators.bodyProposalValidationAlgebra
        .validate(_: BodyProposalValidationContext)(_: BlockBody))
        .expects(*, *)
        .once()
        .onCall { case (_: BodyProposalValidationContext, b: BlockBody) =>
          Validated.validNec[BodySemanticError, BlockBody](b).pure[F]
        }

      type AuthContext = IoTransaction => DynamicContext[F, String, Datum]
      (validators.bodyAuthorization
        .validate(_: AuthContext)(_: BlockBody))
        .expects(*, *)
        .rep(1)
        .onCall { case (_: AuthContext @unchecked, b: BlockBody) =>
          Validated.validNec[BodyAuthorizationError, BlockBody](b).pure[F]
        }

      val slotDataStoreData = knowSlotData.toMap
      (slotDataStore.get).expects(*).anyNumberOfTimes().onCall((id: BlockId) => slotDataStoreData.get(id).pure[F])
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreData(id).pure[F]
        }

      val lastAdoptedBlockSlotData = requestIdSlotDataHeaderBlock.head._2
      (localChain.isWorseThan).expects(*).anyNumberOfTimes().onCall { (ids: NonEmptyChain[SlotData]) =>
        assert(lastAdoptedBlockSlotData == ids.last)
        (lastAdoptedBlockSlotData == ids.last).pure[F]
      }
      (localChain.adopt).expects(Validated.Valid(lastAdoptedBlockSlotData)).once().returning(().pure[F])

      (requestsProxy.sendNoWait)
        .expects(
          RequestsProxy.Message.DownloadBodiesRequest(hostId, NonEmptyChain.one(requestIdSlotDataHeaderBlock(1)._3))
        )

      BlockChecker
        .makeActor(
          requestsProxy,
          localChain,
          slotDataStore,
          headerStore,
          bodyStore,
          validators,
          chainSelectionAlgebra,
          Resource.pure(ed255Vrf),
          defaultConfig,
          Option(BestChain(NonEmptyChain.fromSeq(allIdSlotDataHeaderBlock.map(_._2)).get)),
          Option(hostId)
        )
        .use { actor =>
          for {
            _ <- actor.send(message)
          } yield ()
        }
    }
  }

  test("RemoteBlockBodies: Invalidate block on current best chain do clear best chain and host") {
    withMock {
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val validators = getValidators()
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

      val currentBestChain = arbitraryLinkedSlotDataChain.arbitrary.first
      val invalidSlotData = currentBestChain.get(Gen.choose(0L, currentBestChain.size - 1).first).get

      (requestsProxy.sendNoWait).expects(RequestsProxy.Message.ResetRequestsProxy).returns(().pure[F])

      BlockChecker
        .makeActor(
          requestsProxy,
          localChain,
          slotDataStore,
          headerStore,
          bodyStore,
          validators,
          chainSelectionAlgebra,
          Resource.pure(ed255Vrf),
          defaultConfig,
          Option(BestChain(currentBestChain)),
          Option(hostId)
        )
        .use { actor =>
          for {
            state <- actor.send(
              BlockChecker.Message.InvalidateBlockIds(NonEmptyChain.one(invalidSlotData.slotId.blockId))
            )
            _ = assert(state.bestKnownRemoteSlotDataOpt.isEmpty)
            _ = assert(state.bestKnownRemoteSlotDataHost.isEmpty)
          } yield ()
        }
    }
  }

  test("RemoteBlockBodies: Invalidate block on current best chain and non best chain do clear best chain and host") {
    withMock {
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val validators = getValidators()
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

      val currentBestChain = arbitraryLinkedSlotDataChain.arbitrary.first
      val invalidSlotData = currentBestChain.get(Gen.choose(0L, currentBestChain.size - 1).first).get

      (requestsProxy.sendNoWait).expects(RequestsProxy.Message.ResetRequestsProxy).returns(().pure[F])

      val invalidatedBlocks = NonEmptyChain(arbitraryBlockId.arbitrary.first, invalidSlotData.slotId.blockId)
      BlockChecker
        .makeActor(
          requestsProxy,
          localChain,
          slotDataStore,
          headerStore,
          bodyStore,
          validators,
          chainSelectionAlgebra,
          Resource.pure(ed255Vrf),
          defaultConfig,
          Option(BestChain(currentBestChain)),
          Option(hostId)
        )
        .use { actor =>
          for {
            state <- actor.send(
              BlockChecker.Message.InvalidateBlockIds(invalidatedBlocks)
            )
            _ = assert(state.bestKnownRemoteSlotDataOpt.isEmpty)
            _ = assert(state.bestKnownRemoteSlotDataHost.isEmpty)
          } yield ()
        }
    }
  }

  test("RemoteBlockBodies: Invalidate block on not current best clear as well") {
    withMock {
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val validators = getValidators()
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, BlockId, SlotData]]

      val currentBestChain = arbitraryLinkedSlotDataChain.arbitrary.first
      val invalidBlockId = arbitrarySlotData.arbitrary.first.slotId.blockId

      (requestsProxy.sendNoWait).expects(RequestsProxy.Message.ResetRequestsProxy).once().returns(().pure[F])

      BlockChecker
        .makeActor(
          requestsProxy,
          localChain,
          slotDataStore,
          headerStore,
          bodyStore,
          validators,
          chainSelectionAlgebra,
          Resource.pure(ed255Vrf),
          defaultConfig,
          Option(BestChain(currentBestChain)),
          Option(hostId)
        )
        .use { actor =>
          for {
            state <- actor.send(BlockChecker.Message.InvalidateBlockIds(NonEmptyChain.one(invalidBlockId)))
            _ = assert(state.bestKnownRemoteSlotDataOpt.isEmpty)
            _ = assert(state.bestKnownRemoteSlotDataHost.isEmpty)
          } yield ()
        }
    }
  }
}
