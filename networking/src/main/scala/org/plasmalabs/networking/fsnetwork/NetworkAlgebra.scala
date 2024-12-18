package org.plasmalabs.networking.fsnetwork

import cats.Parallel
import cats.effect.{Async, Resource}
import com.github.benmanes.caffeine.cache.Caffeine
import org.plasmalabs.algebras.{ClockAlgebra, Stats, Store}
import org.plasmalabs.blockchain.Validators
import org.plasmalabs.consensus.algebras.*
import org.plasmalabs.consensus.models.{BlockHeader, BlockId, SlotData}
import org.plasmalabs.crypto.signing.Ed25519VRF
import org.plasmalabs.eventtree.ParentChildTree
import org.plasmalabs.ledger.algebras.*
import org.plasmalabs.models.p2p.*
import org.plasmalabs.networking.blockchain.BlockchainPeerClient
import org.plasmalabs.networking.fsnetwork.BlockChecker.BlockCheckerActor
import org.plasmalabs.networking.fsnetwork.Notifier.NotifierActor
import org.plasmalabs.networking.fsnetwork.PeerActor.PeerActor
import org.plasmalabs.networking.fsnetwork.PeerBlockBodyFetcher.PeerBlockBodyFetcherActor
import org.plasmalabs.networking.fsnetwork.PeerBlockHeaderFetcher.PeerBlockHeaderFetcherActor
import org.plasmalabs.networking.fsnetwork.PeerMempoolTransactionSync.PeerMempoolTransactionSyncActor
import org.plasmalabs.networking.fsnetwork.PeersManager.PeersManagerActor
import org.plasmalabs.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.sdk.models.TransactionId
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.validation.algebras.TransactionSyntaxVerifier
import org.typelevel.log4cats.Logger

// scalastyle:off parameter.number
trait NetworkAlgebra[F[_]] {

  def makePeerManger(
    thisHostId:                  HostId,
    networkAlgebra:              NetworkAlgebra[F],
    localChain:                  LocalChainAlgebra[F],
    chainSelection:              ChainSelectionAlgebra[F, BlockId, SlotData],
    slotDataStore:               Store[F, BlockId, SlotData],
    bodyStore:                   Store[F, BlockId, BlockBody],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    blockIdTree:                 ParentChildTree[F, BlockId],
    mempool:                     MempoolAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    newPeerCreationAlgebra:      PeerCreationRequestAlgebra[F],
    p2pNetworkConfig:            P2PNetworkConfig,
    hotPeersUpdate:              Set[RemotePeer] => F[Unit],
    savePeersFunction:           Set[KnownRemotePeer] => F[Unit],
    ed25519VRF:                  Resource[F, Ed25519VRF]
  ): Resource[F, PeersManagerActor[F]]

  def makeBlockChecker(
    requestsProxy:         RequestsProxyActor[F],
    localChain:            LocalChainAlgebra[F],
    slotDataStore:         Store[F, BlockId, SlotData],
    headerStore:           Store[F, BlockId, BlockHeader],
    bodyStore:             Store[F, BlockId, BlockBody],
    validators:            Validators[F],
    chainSelectionAlgebra: ChainSelectionAlgebra[F, BlockId, SlotData],
    ed25519VRF:            Resource[F, Ed25519VRF],
    p2pNetworkConfig:      P2PNetworkConfig
  ): Resource[F, BlockCheckerActor[F]]

  def makeRequestsProxy(
    peersManager: PeersManagerActor[F],
    headerStore:  Store[F, BlockId, BlockHeader],
    bodyStore:    Store[F, BlockId, BlockBody]
  ): Resource[F, RequestsProxyActor[F]]

  def makeNotifier(
    peersManager:     PeersManagerActor[F],
    p2pNetworkConfig: P2PNetworkConfig
  ): Resource[F, NotifierActor[F]]

  def makePeer(
    hostId:                      HostId,
    networkAlgebra:              NetworkAlgebra[F],
    client:                      BlockchainPeerClient[F],
    requestsProxy:               RequestsProxyActor[F],
    peersManager:                PeersManagerActor[F],
    localChain:                  LocalChainAlgebra[F],
    chainSelection:              ChainSelectionAlgebra[F, BlockId, SlotData],
    slotDataStore:               Store[F, BlockId, SlotData],
    bodyStore:                   Store[F, BlockId, BlockBody],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    mempool:                     MempoolAlgebra[F],
    commonAncestorF:             CommonAncestorF[F],
    ed25519VRF:                  Resource[F, Ed25519VRF],
    blockIdTree:                 ParentChildTree[F, BlockId]
  ): Resource[F, PeerActor[F]]

  def makePeerHeaderFetcher(
    hostId:          HostId,
    client:          BlockchainPeerClient[F],
    requestsProxy:   RequestsProxyActor[F],
    peersManager:    PeersManagerActor[F],
    localChain:      LocalChainAlgebra[F],
    chainSelection:  ChainSelectionAlgebra[F, BlockId, SlotData],
    slotDataStore:   Store[F, BlockId, SlotData],
    bodyStore:       Store[F, BlockId, BlockBody],
    commonAncestorF: CommonAncestorF[F],
    ed25519VRF:      Resource[F, Ed25519VRF],
    blockIdTree:     ParentChildTree[F, BlockId]
  ): Resource[F, PeerBlockHeaderFetcherActor[F]]

  def makePeerBodyFetcher(
    hostId:                      HostId,
    client:                      BlockchainPeerClient[F],
    requestsProxy:               RequestsProxyActor[F],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F]
  ): Resource[F, PeerBlockBodyFetcherActor[F]]

  def makeMempoolSyncFetcher(
    hostId:                      HostId,
    client:                      BlockchainPeerClient[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    mempool:                     MempoolAlgebra[F],
    peersManager:                PeersManagerActor[F],
    localChainAlgebra:           LocalChainAlgebra[F]
  ): Resource[F, PeerMempoolTransactionSyncActor[F]]
}

class NetworkAlgebraImpl[F[_]: Async: Parallel: Logger: DnsResolver: ReverseDnsResolver: Stats](
  clock: ClockAlgebra[F]
) extends NetworkAlgebra[F] {

  override def makePeerManger(
    thisHostId:                  HostId,
    networkAlgebra:              NetworkAlgebra[F],
    localChain:                  LocalChainAlgebra[F],
    chainSelection:              ChainSelectionAlgebra[F, BlockId, SlotData],
    slotDataStore:               Store[F, BlockId, SlotData],
    bodyStore:                   Store[F, BlockId, BlockBody],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    blockIdTree:                 ParentChildTree[F, BlockId],
    mempool:                     MempoolAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    newPeerCreationAlgebra:      PeerCreationRequestAlgebra[F],
    p2pNetworkConfig:            P2PNetworkConfig,
    hotPeersUpdate:              Set[RemotePeer] => F[Unit],
    savePeersFunction:           Set[KnownRemotePeer] => F[Unit],
    ed25519VRF:                  Resource[F, Ed25519VRF]
  ): Resource[F, PeersManagerActor[F]] = {
    val coldToWarm: SelectorColdToWarm[F] = new SemiRandomSelectorColdToWarm[F]()
    val warmToHot: SelectorWarmToHot[F] = new ReputationRandomBasedSelectorWarmToHot[F]()

    PeersManager.makeActor(
      thisHostId,
      networkAlgebra,
      localChain,
      chainSelection,
      slotDataStore,
      bodyStore,
      transactionStore,
      blockIdTree,
      mempool,
      headerToBodyValidation,
      transactionSyntaxValidation,
      newPeerCreationAlgebra,
      p2pNetworkConfig,
      hotPeersUpdate,
      savePeersFunction,
      coldToWarm,
      warmToHot,
      ed25519VRF,
      initialPeers = Map.empty[HostId, Peer[F]],
      blockSource = Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
    )
  }

  override def makeBlockChecker(
    requestsProxy:         RequestsProxyActor[F],
    localChain:            LocalChainAlgebra[F],
    slotDataStore:         Store[F, BlockId, SlotData],
    headerStore:           Store[F, BlockId, BlockHeader],
    bodyStore:             Store[F, BlockId, BlockBody],
    validators:            Validators[F],
    chainSelectionAlgebra: ChainSelectionAlgebra[F, BlockId, SlotData],
    ed25519VRF:            Resource[F, Ed25519VRF],
    p2pNetworkConfig:      P2PNetworkConfig
  ): Resource[F, BlockCheckerActor[F]] =
    BlockChecker.makeActor(
      requestsProxy,
      localChain,
      slotDataStore,
      headerStore,
      bodyStore,
      validators,
      chainSelectionAlgebra,
      ed25519VRF,
      p2pNetworkConfig
    )

  override def makeRequestsProxy(
    peersManager: PeersManagerActor[F],
    headerStore:  Store[F, BlockId, BlockHeader],
    bodyStore:    Store[F, BlockId, BlockBody]
  ): Resource[F, RequestsProxyActor[F]] =
    RequestsProxy.makeActor(peersManager, headerStore, bodyStore)

  override def makeNotifier(
    peersManager:     PeersManagerActor[F],
    p2pNetworkConfig: P2PNetworkConfig
  ): Resource[F, NotifierActor[F]] =
    Notifier.makeActor(peersManager, p2pNetworkConfig)

  override def makePeer(
    hostId:                      HostId,
    networkAlgebra:              NetworkAlgebra[F],
    client:                      BlockchainPeerClient[F],
    requestsProxy:               RequestsProxyActor[F],
    peersManager:                PeersManagerActor[F],
    localChain:                  LocalChainAlgebra[F],
    chainSelection:              ChainSelectionAlgebra[F, BlockId, SlotData],
    slotDataStore:               Store[F, BlockId, SlotData],
    bodyStore:                   Store[F, BlockId, BlockBody],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    mempool:                     MempoolAlgebra[F],
    commonAncestorF:             CommonAncestorF[F],
    ed25519VRF:                  Resource[F, Ed25519VRF],
    blockIdTree:                 ParentChildTree[F, BlockId]
  ): Resource[F, PeerActor[F]] =
    PeerActor.makeActor(
      hostId,
      networkAlgebra,
      client,
      requestsProxy,
      peersManager,
      localChain,
      chainSelection,
      slotDataStore,
      bodyStore,
      transactionStore,
      headerToBodyValidation,
      transactionSyntaxValidation,
      mempool,
      commonAncestorF,
      ed25519VRF,
      blockIdTree
    )

  override def makePeerHeaderFetcher(
    hostId:          HostId,
    client:          BlockchainPeerClient[F],
    requestsProxy:   RequestsProxyActor[F],
    peersManager:    PeersManagerActor[F],
    localChain:      LocalChainAlgebra[F],
    chainSelection:  ChainSelectionAlgebra[F, BlockId, SlotData],
    slotDataStore:   Store[F, BlockId, SlotData],
    bodyStore:       Store[F, BlockId, BlockBody],
    commonAncestorF: CommonAncestorF[F],
    ed25519VRF:      Resource[F, Ed25519VRF],
    blockIdTree:     ParentChildTree[F, BlockId]
  ): Resource[F, PeerBlockHeaderFetcherActor[F]] =
    PeerBlockHeaderFetcher.makeActor(
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

  override def makePeerBodyFetcher(
    hostId:                      HostId,
    client:                      BlockchainPeerClient[F],
    requestsProxy:               RequestsProxyActor[F],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F]
  ): Resource[F, PeerBlockBodyFetcherActor[F]] =
    PeerBlockBodyFetcher.makeActor(
      hostId,
      client,
      requestsProxy,
      transactionStore,
      headerToBodyValidation,
      transactionSyntaxValidation
    )

  override def makeMempoolSyncFetcher(
    hostId:                      HostId,
    client:                      BlockchainPeerClient[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    mempool:                     MempoolAlgebra[F],
    peersManager:                PeersManagerActor[F],
    localChainAlgebra:           LocalChainAlgebra[F]
  ): Resource[F, PeerMempoolTransactionSyncActor[F]] =
    PeerMempoolTransactionSync.makeActor(
      hostId,
      client,
      transactionSyntaxValidation,
      transactionStore,
      mempool,
      peersManager,
      localChainAlgebra
    )
}
// scalastyle:on parameter.number
