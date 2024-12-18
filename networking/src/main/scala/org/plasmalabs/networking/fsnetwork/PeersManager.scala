package org.plasmalabs.networking.fsnetwork

import cats.data.{NonEmptyChain, OptionT}
import cats.effect.implicits.effectResourceOps
import cats.effect.{Async, Resource}
import cats.implicits.*
import cats.{Parallel, Show}
import com.github.benmanes.caffeine.cache.Cache
import org.plasmalabs.actor.{Actor, Fsm}
import org.plasmalabs.algebras.{Stats, Store}
import org.plasmalabs.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import org.plasmalabs.consensus.algebras.{BlockHeaderToBodyValidationAlgebra, ChainSelectionAlgebra, LocalChainAlgebra}
import org.plasmalabs.consensus.models.{BlockHeader, BlockId, SlotData}
import org.plasmalabs.crypto.signing.Ed25519VRF
import org.plasmalabs.eventtree.ParentChildTree
import org.plasmalabs.ledger.algebras.MempoolAlgebra
import org.plasmalabs.models.p2p.*
import org.plasmalabs.models.utility.*
import org.plasmalabs.networking.*
import org.plasmalabs.networking.blockchain.BlockchainPeerClient
import org.plasmalabs.networking.fsnetwork.BlockChecker.BlockCheckerActor
import org.plasmalabs.networking.fsnetwork.DnsResolverHTInstances.*
import org.plasmalabs.networking.fsnetwork.P2PShowInstances.*
import org.plasmalabs.networking.fsnetwork.PeerActor.PeerActor
import org.plasmalabs.networking.fsnetwork.PeersManager.Message.*
import org.plasmalabs.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import org.plasmalabs.networking.fsnetwork.ReverseDnsResolverHTInstances.*
import org.plasmalabs.networking.p2p.*
import org.plasmalabs.node.models.{BlockBody, KnownHost}
import org.plasmalabs.sdk.models.TransactionId
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.validation.algebras.TransactionSyntaxVerifier
import org.plasmalabs.typeclasses.implicits.*
import org.typelevel.log4cats.Logger
import scodec.bits.ByteVector

/**
 * Actor for managing peers
 */
// scalastyle:off number.of.methods
object PeersManager {
  sealed trait Message

  object Message {

    /**
     * Notify Peer manager about local used address
     * @param localAddress used local address
     */
    case class UpdateThisPeerAddress(localAddress: RemoteAddress) extends Message

    /**
     * Setup appropriate actor for connection to peer specified by hostId, client is created outside before
     * @param client client with already opened connection to host
     * @param remotePeerAsServer remote peer presentation as server
     * @tparam F effect
     */
    case class OpenedPeerConnection[F[_]](client: BlockchainPeerClient[F], remotePeerAsServer: Option[KnownHost])
        extends Message

    /**
     * Set block checker actor, can't be done in constructor due cyclic references
     * @param blockChecker block checker
     * @tparam F effect
     */
    case class SetupBlockChecker[F[_]](blockChecker: BlockCheckerActor[F]) extends Message

    /**
     * Set requests proxy actor, can't be done in constructor due cyclic references
     *
     * @param requestsProxy block checker
     * @tparam F effect
     */
    case class SetupRequestsProxy[F[_]](requestsProxy: RequestsProxyActor[F]) extends Message

    /**
     * PingPong message from remote peer, which allow us to measure performance reputation for remote host
     * without exchange any application level information
     *
     * @param hostId   remote peer
     * @param response ping pong message response
     */
    case class PingPongMessagePing(hostId: HostId, response: Either[NetworkQualityError, Long]) extends Message

    /**
     * Information about how long it takes to download block header from remote host,
     * allow us to update performance reputation
     *
     * @param hostId remote peer
     * @param delay  header download time
     */
    case class DownloadTimeHeader(hostId: HostId, delay: Long) extends Message

    /**
     * Information about how long it takes to download block body from remote host,
     * allow us to update performance reputation
     *
     * @param hostId    remote peer
     * @param bodyDelay body download time
     * @param txDelays  transactions download time
     */
    case class DownloadTimeBody(hostId: HostId, bodyDelay: Long, txDelays: Seq[Long]) extends Message

    /**
     * Information how many transaction for Memory pool had been received via that host,
     * allow keep connection to remote peer which provide transactions for memory pool
     * @param hostId remote peer
     * @param txCount how many transaction had been received per slot
     */
    case class ReceivedTransactionsCount(hostId: HostId, txCount: Long) extends Message

    /**
     * Remote peer provide us remote slot data with better height than our current local chain,
     * but whole slot data chain turned out to be worse of local chain because of density rule
     *
     * @param hostId remote peer
     */
    case class BadKLookbackSlotData(hostId: HostId) extends Message

    /**
     * Remote peer provide to us incorrect block or any other data like genesis block.
     * For example it could be block with incorrect transaction(s)
     *
     * @param hostId remote peer
     */
    case class CriticalErrorForHost(hostId: HostId) extends Message

    /**
     * We got unknown error during get data from remote peer. That error could be network error, for example
     *
     * @param hostId remote peer
     */
    case class NonCriticalErrorForHost(hostId: HostId) extends Message

    /**
     * Ban peer by some reason like invalid block id
     * @param hostId peer to be affected
     */
    case class BanPeer(hostId: HostId) extends Message

    /**
     * Peer connection for closing, connection will be closed even if remote peer is still use it
     *
     * @param hostId closed peer
     */
    case class ClosePeer(hostId: HostId) extends Message

    /**
     * Move peer to cold status, close connection ONLY if remote peer is not used it as well
     * @param hostIds peer for moving to cold state
     */
    case class MoveToCold(hostIds: NonEmptyChain[HostId]) extends Message

    /**
     * Add known peers to peers list
     *
     * @param knownPeers peers to add
     */
    case class AddKnownPeers(knownPeers: NonEmptyChain[KnownRemotePeer]) extends Message

    /**
     * Forget remote peer, if connection is open to that peer then close that connection as well
     * @param hostId peer to forget
     */
    case class ForgetPeer(hostId: HostId) extends Message

    /**
     * Add known neighbour, i.e. hot peers of remote current hot peer
     * @param knownNeighbors known neighbors
     */
    case class AddKnownNeighbors(source: HostId, knownNeighbors: NonEmptyChain[RemotePeer]) extends Message

    /**
     * @param hostId use hostId as a possible hint
     * @param blockIds list of block's id of headers to be requested from peer
     */
    case class BlockHeadersRequest(hostId: Option[HostId], blockIds: NonEmptyChain[BlockId]) extends Message

    /**
     * @param hostId use hostId as a possible hint
     * @param blockHeaders requested bodies
     */
    case class BlockBodyRequest(hostId: Option[HostId], blockHeaders: NonEmptyChain[BlockHeader]) extends Message

    /**
     * Update block availability on remote peer
     * @param sources block source
     */
    case class BlocksSource(sources: NonEmptyChain[(HostId, BlockId)]) extends Message

    /**
     * Request current tips from all connected hot peers
     */
    case object GetCurrentTips extends Message

    /**
     * Reputation had been updated notification
     */
    case object UpdatedReputationTick extends Message

    /**
     * Request measuring network quality from remote warm hosts
     */
    case object GetNetworkQualityForWarmHosts extends Message

    /**
     * Try to connect to new peers
     */
    case object AggressiveP2PUpdate extends Message

    /**
     * Update warm hosts list
     */
    case object UpdatePeersTick extends Message

    /**
     * Update remote peer application level status
     */
    case class RemotePeerNetworkLevel(hostId: HostId, networkLevel: Boolean) extends Message

    /**
     * Remote peer Id had been changed, for example id was not known initially and we use some dummy id
     * @param oldId old id
     * @param newId actual id for remote peer
     */
    case class RemotePeerIdChanged(oldId: HostId, newId: HostId) extends Message

    /**
     * Print current P2P state including common ancestor for all physically available connections
     */
    case object PrintP2PState extends Message
  }

  case class State[F[_]](
    thisHostId:                  HostId,
    thisHostIps:                 Set[String],
    networkAlgebra:              NetworkAlgebra[F],
    blocksChecker:               Option[BlockCheckerActor[F]],
    requestsProxy:               Option[RequestsProxyActor[F]],
    peersHandler:                PeersHandler[F],
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
    coldToWarmSelector:          SelectorColdToWarm[F],
    warmToHotSelector:           SelectorWarmToHot[F],
    hotPeersUpdate:              Set[RemotePeer] => F[Unit],
    blockSource:                 Cache[BlockId, Set[HostId]],
    peerFilter:                  PeerFilter,
    ed25519VRF:                  Resource[F, Ed25519VRF]
  )

  type Response[F[_]] = State[F]
  type PeersManagerActor[F[_]] = Actor[F, Message, Response[F]]

  // scalastyle:off cyclomatic.complexity
  def getFsm[F[_]: Async: Parallel: Logger: DnsResolver: ReverseDnsResolver: Stats]
    : PeersManagerActor[F] => Fsm[F, State[F], Message, Response[F]] =
    (thisActor: PeersManagerActor[F]) =>
      Fsm {
        case (state, checker: SetupBlockChecker[F] @unchecked)    => setupBlockChecker(state, checker.blockChecker)
        case (state, checker: SetupRequestsProxy[F] @unchecked)   => setupRequestsProxy(state, checker.requestsProxy)
        case (state, PingPongMessagePing(hostId, pongResponse))   => pongMessage(thisActor, state, hostId, pongResponse)
        case (state, DownloadTimeHeader(hostId, delay))           => headerDownloadTime(state, hostId, delay)
        case (state, DownloadTimeBody(hostId, delay, txDelays))   => blockDownloadTime(state, hostId, delay, txDelays)
        case (state, ReceivedTransactionsCount(hostId, txCount))  => updateTransactionRep(state, hostId, txCount)
        case (state, BadKLookbackSlotData(hostId))                => badKLookbackSlotData(thisActor, state, hostId)
        case (state, CriticalErrorForHost(hostId))                => criticalErrorForHost(thisActor, state, hostId)
        case (state, NonCriticalErrorForHost(hostId))             => nonCriticalErrorForHost(thisActor, state, hostId)
        case (state, BlockHeadersRequest(hostId, blocks))         => blockHeadersRequest(state, hostId, blocks)
        case (state, BlockBodyRequest(hostId, blockHeaders))      => blockDownloadRequest(state, hostId, blockHeaders)
        case (state, BlocksSource(sources))                       => blocksSourceProcessing(state, sources)
        case (state, GetNetworkQualityForWarmHosts)               => doNetworkQualityMeasure(state)
        case (state, PrintP2PState)                               => printP2PState(thisActor, state)
        case (state, GetCurrentTips)                              => getCurrentTips(state)
        case (state, RemotePeerNetworkLevel(peer, level))         => remoteNetworkLevel(thisActor, state, peer, level)
        case (state, RemotePeerIdChanged(oldId, newId))           => remoteIdChanged(thisActor, state, oldId, newId)
        case (state, MoveToCold(peers))                           => coldPeer(thisActor, state, peers)
        case (state, ClosePeer(peer))                             => closePeer(thisActor, state, peer)
        case (state, BanPeer(hostId))                             => banPeer(thisActor, state, hostId)
        case (state, UpdateThisPeerAddress(localAddress))         => addLocalAddress(state, localAddress)
        case (state, newPeer: OpenedPeerConnection[F] @unchecked) => openedPeerConnection(thisActor, state, newPeer)
        case (state, AddKnownNeighbors(source, peers))            => addKnownNeighbors(state, source, peers)
        case (state, AddKnownPeers(peers))                        => addKnownPeers(state, peers)
        case (state, ForgetPeer(peer))                            => forgetPeer(thisActor, state, peer)
        case (state, UpdatePeersTick)                             => updatePeersTick(state)
        case (state, AggressiveP2PUpdate)                         => aggressiveP2PUpdate(thisActor, state)
        case (state, UpdatedReputationTick)                       => repUpdate(thisActor, state)
      }
  // scalastyle:on cyclomatic.complexity

  // scalastyle:off parameter.number
  def makeActor[F[_]: Async: Parallel: Logger: DnsResolver: ReverseDnsResolver: Stats](
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
    p2pConfig:                   P2PNetworkConfig,
    hotPeersUpdate:              Set[RemotePeer] => F[Unit],
    savePeersFunction:           Set[KnownRemotePeer] => F[Unit],
    coldToWarmSelector:          SelectorColdToWarm[F],
    warmToHotSelector:           SelectorWarmToHot[F],
    ed25519VRF:                  Resource[F, Ed25519VRF],
    initialPeers:                Map[HostId, Peer[F]],
    blockSource:                 Cache[BlockId, Set[HostId]]
  ): Resource[F, PeersManagerActor[F]] =
    for {
      _         <- Logger[F].info(show"Start PeerManager for host $thisHostId").toResource
      actorName <- "Peers manager actor".pure[F].toResource

      selfBannedPeer =
        thisHostId -> Peer[F](
          PeerState.Banned,
          None,
          None,
          None,
          Seq.empty,
          remoteNetworkLevel = false,
          0,
          0,
          0,
          0,
          None
        )

      filteredHosts <- parseHostIds(p2pConfig.networkProperties.doNotExposeIds).toResource
      peerFilter    <- new PeerFilter(p2pConfig.networkProperties.doNotExposeIps, filteredHosts).pure[F].toResource

      initialState =
        PeersManager.State[F](
          thisHostId,
          Set.empty,
          networkAlgebra,
          blocksChecker = None,
          requestsProxy = None,
          PeersHandler(initialPeers + selfBannedPeer, p2pConfig),
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
          p2pConfig,
          coldToWarmSelector,
          warmToHotSelector,
          hotPeersUpdate,
          blockSource,
          peerFilter,
          ed25519VRF
        )

      actor <- Actor.makeFull(actorName, initialState, getFsm[F], finalizeActor[F](savePeersFunction))
    } yield actor
  // scalastyle:on parameter.number

  private def parseHostIds[F[_]: Async: Logger](hosts: Seq[String]): F[Seq[HostId]] =
    for {
      parseRes <- hosts.map(id => ByteVector.fromBase58Descriptive(id)).pure[F]
      _ <- parseRes.traverse {
        case Left(error) => Logger[F].error(s"Failed to extract filtering host from Base58: $error")
        case _           => ().pure[F]
      }
      filtered <- parseRes.flatMap(_.toOption).map(id => HostId(id)).pure[F]
    } yield filtered

  private def finalizeActor[F[_]: Async: Logger](
    savePeersFunction: Set[KnownRemotePeer] => F[Unit]
  )(state: State[F])(implicit res: ReverseDnsResolverHT[KnownRemotePeer, F]): F[Unit] =
    for {
      ipToSave  <- state.peersHandler.getRemotePeers.toSeq.pure[F]
      _         <- Logger[F].debug(show"Try to resolve next ip(s) as hostnames: $ipToSave")
      hostNames <- state.peersHandler.getRemotePeers.toSeq.traverse(_.reverseResolving())
      _         <- Logger[F].debug(show"Resolved ip(s) as hostnames: $hostNames")
      _         <- savePeersFunction(hostNames.toSet)
    } yield ()

  private def peerReleaseAction[F[_]: Async](thisActor: PeersManagerActor[F])(peer: Peer[F]): F[Unit] =
    peer.actorOpt.traverse_(a => thisActor.releaseActor(a).void)

  private def setupBlockChecker[F[_]: Async: Logger](
    state:         State[F],
    blocksChecker: BlockCheckerActor[F]
  ): F[(State[F], Response[F])] = {
    val newState = state.copy(blocksChecker = Option(blocksChecker))
    Logger[F].info("Setup block checker for PeerManager") >>
    (newState, newState).pure[F]
  }

  private def setupRequestsProxy[F[_]: Async: Logger](
    state:         State[F],
    requestsProxy: RequestsProxyActor[F]
  ): F[(State[F], Response[F])] = {
    val newState = state.copy(requestsProxy = Option(requestsProxy))
    Logger[F].info("Setup requests proxy for PeerManager") >>
    (newState, newState).pure[F]
  }

  def delayToReputation(networkConfig: P2PNetworkConfig, delayInMs: Long): HostReputationValue = {
    val reputationReducing = delayInMs.toDouble / networkConfig.performanceReputationMaxDelay

    networkConfig.performanceReputationIdealValue - reputationReducing
  }

  private def pongMessage[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostId:    HostId,
    response:  Either[NetworkQualityError, Long]
  ): F[(State[F], Response[F])] =
    response match {
      case Right(value) =>
        val newReputation = delayToReputation(state.p2pNetworkConfig, value)
        val newPeersHandler = state.peersHandler.copyWithUpdatedReputation(perfRepMap = Map(hostId -> newReputation))
        val newState = state.copy(peersHandler = newPeersHandler)
        Logger[F].info(show"Got pong message delay $value ms from remote host $hostId") >>
        (newState, newState).pure[F]

      case Left(error) =>
        Logger[F].error(show"Bad pong message: $error from host $hostId") >>
        banPeer(thisActor, state, hostId)
    }

  private def updatePerfRepWithDelay[F[_]: Async](
    state:  State[F],
    hostId: HostId,
    delay:  Long
  ): F[(State[F], Response[F])] = {
    val newReputation = delayToReputation(state.p2pNetworkConfig, delay)
    val oldReputation = state.peersHandler.get(hostId).map(_.perfRep)
    val updatedReputation =
      oldReputation.map(oldValue => (oldValue * 2 + newReputation) / 3.0).getOrElse(newReputation)

    val newPeersHandler =
      state.peersHandler.copyWithUpdatedReputation(perfRepMap = Map(hostId -> updatedReputation))
    val newState = state.copy(peersHandler = newPeersHandler)

    (newState, newState).pure[F]
  }

  private def headerDownloadTime[F[_]: Async: Logger: Stats](
    state:  State[F],
    hostId: HostId,
    delay:  Long
  ): F[(State[F], Response[F])] =
    Async[F].defer(
      Stats[F].recordGauge(
        "plasma_node_header_download_time",
        "Header download time from other peer in millisecconds",
        Map("from_host" -> stringToJson(show"${hostId.id}")),
        longToJson(delay)
      )
    ) >>
    Logger[F].info(show"Received header download from host $hostId with delay $delay ms") >>
    updatePerfRepWithDelay(state, hostId, delay)

  private def blockDownloadTime[F[_]: Async: Logger: Stats](
    state:    State[F],
    hostId:   HostId,
    delay:    Long,
    tsDelays: Seq[Long]
  ): F[(State[F], Response[F])] = {
    val maxDelay = (tsDelays :+ delay).max
    Async[F].defer(
      Stats[F].recordGauge(
        "plasma_node_block_download_time",
        "Block download time from other peer in millisecconds",
        Map("from_host" -> stringToJson(show"${hostId.id}")),
        longToJson(delay)
      )
    ) >>
    Logger[F].debug(show"Received block download from host $hostId with max delay $delay ms") >>
    updatePerfRepWithDelay(state, hostId, maxDelay)
  }

  private def updateTransactionRep[F[_]: Async](
    state:   State[F],
    hostId:  HostId,
    txCount: Long
  ): F[(State[F], Response[F])] = {
    val prevRep = state.peersHandler.get(hostId).fold(0.0)(_.mempoolTxRep)
    val txImpactRatio = state.p2pNetworkConfig.networkProperties.txImpactRatio
    val newRep = ((txImpactRatio - 1) * prevRep + txCount) / txImpactRatio
    val newHandler = state.peersHandler.copyWithUpdatedReputation(mempoolTxRepMap = Map(hostId -> newRep))
    val newState = state.copy(peersHandler = newHandler)
    (newState, newState).pure[F]
  }

  private def badKLookbackSlotData[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostId:    HostId
  ): F[(State[F], Response[F])] =
    Logger[F].error(show"Got bad k lookback slot data from host $hostId") >>
    coldPeer(thisActor, state, NonEmptyChain.one(hostId))

  private def criticalErrorForHost[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostId:    HostId
  ): F[(State[F], Response[F])] =
    Logger[F].error(show"Got critical error from host $hostId") >>
    banPeer(thisActor, state, hostId)

  private def nonCriticalErrorForHost[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostId:    HostId
  ): F[(State[F], Response[F])] =
    Logger[F].error(show"Got non critical error from host $hostId") >>
    coldPeer(thisActor, state, NonEmptyChain.one(hostId))

  private def blockHeadersRequest[F[_]: Async: Logger](
    state:    State[F],
    hostId:   Option[HostId],
    blockIds: NonEmptyChain[BlockId]
  ): F[(State[F], Response[F])] =
    // request is done for linked blocks, i.e. last block is child for any other block in request,
    // thus we could use it for detect block source. TODO make request parallel
    getHotPeerByBlockId(state, blockIds.last, hostId) match {
      case Some((source, peer)) =>
        for {
          _              <- Logger[F].info(show"Forward to $source download block header(s) $blockIds")
          _              <- peer.sendNoWait(PeerActor.Message.DownloadBlockHeaders(blockIds))
          newRep         <- (source -> Math.min(peer.newRep + 1, state.p2pNetworkConfig.maxPeerNovelty)).pure[F]
          newPeerHandler <- state.peersHandler.copyWithUpdatedReputation(noveltyRepMap = Map(newRep)).pure[F]
          newState       <- state.copy(peersHandler = newPeerHandler).pure[F]
        } yield (newState, newState)
      case None =>
        Logger[F].error(show"Failed to get source host for getting header(s) $blockIds") >>
        state.blocksChecker
          .traverse_(_.sendNoWait(BlockChecker.Message.InvalidateBlockIds(NonEmptyChain.one(blockIds.last)))) >>
        (state, state).pure[F]
    }

  private def blockDownloadRequest[F[_]: Async: Logger](
    state:  State[F],
    hostId: Option[HostId],
    blocks: NonEmptyChain[BlockHeader]
  ): F[(State[F], Response[F])] =
    // request is done for linked blocks, i.e. last block is child for any other block in request,
    // thus we could use it for detect block source. TODO make request parallel
    getHotPeerByBlockId(state, blocks.last.id, hostId) match {
      case Some((source, peer)) =>
        Logger[F].info(show"Forward to $source download block body(s) ${blocks.map(_.id).toList}")
        peer.sendNoWait(PeerActor.Message.DownloadBlockBodies(blocks)) >>
        (state, state).pure[F]
      case None =>
        Logger[F].error(show"Failed to get source host for getting body(s) ${blocks.map(_.id).toList}") >>
        state.blocksChecker
          .traverse_(_.sendNoWait(BlockChecker.Message.InvalidateBlockIds(NonEmptyChain.one(blocks.last.id)))) >>
        (state, state).pure[F]
    }

  private def getHotPeerByBlockId[F[_]](
    state:   State[F],
    blockId: BlockId,
    hostId:  Option[HostId]
  ): Option[(HostId, Peer[F])] =
    for {
      sources <- Option(state.blockSource.getOrElse(blockId, Set.empty) ++ hostId.toSet)
      source  <- state.peersHandler.getHotPeers.filter(_._2.haveConnection).keySet.find(sources.contains)
      peer    <- state.peersHandler.get(source)
    } yield (source, peer)

  def knownSourcesToReputation(networkConfig: P2PNetworkConfig, knownSources: Long): HostReputationValue = {
    val reputationReducing: HostReputationValue = (knownSources - 1) * networkConfig.blockNoveltyReputationStep
    Math.max(networkConfig.blockNoveltyInitialValue - reputationReducing, 0)
  }

  private def blocksSourceProcessing[F[_]: Async](
    state:   State[F],
    sources: NonEmptyChain[(HostId, BlockId)]
  ): F[(State[F], Response[F])] = {
    val peerToKnownSource: Map[HostId, Int] =
      sources.toList
        .flatMap { case (host, blockId) =>
          val previousSource: Set[HostId] = state.blockSource.get(blockId).getOrElse(Set.empty[HostId])
          if (previousSource.contains(host)) {
            Option.empty[(HostId, Int)]
          } else {
            val newSource: Set[HostId] = previousSource + host
            state.blockSource.put(blockId, newSource)
            Option((host, newSource.size))
          }
        }
        .groupMapReduce(_._1)(_._2)(Math.min)

    val perfRepUpdate: Map[HostId, HostReputationValue] =
      peerToKnownSource
        .map { case (source, knownSourceCount) =>
          val newReputation = knownSourcesToReputation(state.p2pNetworkConfig, knownSourceCount)
          val oldReputation = state.peersHandler.get(source).fold(0.0)(_.blockRep)
          (source, Math.max(newReputation, oldReputation))
        }

    val noveltyRepUpdate =
      peerToKnownSource
        .filter(_._2 == 1) // select peer which provide completely new block
        .map { case (source, oldRep) => source -> Math.max(oldRep, state.p2pNetworkConfig.remotePeerNoveltyInSlots) }

    val newPeerHandler =
      state.peersHandler.copyWithUpdatedReputation(blockRepMap = perfRepUpdate, noveltyRepMap = noveltyRepUpdate)
    val newState = state.copy(peersHandler = newPeerHandler)

    (newState, newState).pure[F]
  }

  private def doNetworkQualityMeasure[F[_]: Async](state: State[F]): F[(State[F], Response[F])] =
    state.peersHandler.getWarmPeersWithActor.values.toSeq.traverse(_.sendNoWait(PeerActor.Message.GetNetworkQuality)) >>
    (state, state).pure[F]

  private def printP2PState[F[_]: Async: Logger: Stats](
    thisActor: PeersManagerActor[F],
    state:     State[F]
  ): F[(State[F], State[F])] = {
    val hotPeers = state.peersHandler.getHotPeers
    val hotPeersIds = hotPeers.keySet.toList.map(showHostId.show).sorted.mkString(",")
    val warmPeers = state.peersHandler.getWarmPeers
    val coldPeers = state.peersHandler.getColdPeers
    val hotPeerAttributes = Map("host_id" -> state.thisHostId.show) ++ hotPeers.map("peer_id" -> _._1.show)

    Logger[F].info(show"This peer id: ${state.thisHostId}") >>
    state.localChain.head.map(head => Logger[F].info(show"Current head: ${head.slotId}")) >>
    Logger[F].info(show"Known local addresses: ${state.thisHostIps}") >>
    Logger[F].info(show"${state.thisHostId}: Current (${hotPeers.size}) hot peer(s) state: $hotPeers") >>
    Logger[F].info(show"Hot peers sum for ${state.thisHostId}: $hotPeersIds") >>
    Logger[F].info(show"Current (${warmPeers.size}) warm peer(s) state: $warmPeers") >>
    Logger[F].info(show"Current first five of (${coldPeers.size}) cold peer(s) state: ${coldPeers.take(5)}") >>
    Logger[F].info(show"With known cold peers: ${coldPeers.map(d => d._1 -> d._2.asServer)}") >>
    printQueueSizeInfo(thisActor, state) >>
    Async[F].defer(
      Stats[F].recordGauge(
        "plasma_node_hot_peers_count",
        "Number of peers in hot state for given node.",
        stringMapToJsonMap(hotPeerAttributes),
        longToJson(hotPeers.size)
      )
    ) >>
    Async[F].defer(
      Stats[F].recordGauge(
        "plasma_node_warm_peers_count",
        "Number of peers in warm state for given node.",
        Map("host_id" -> stringToJson(show"${state.thisHostId}")),
        longToJson(warmPeers.size)
      )
    ) >>
    Async[F].defer(
      Stats[F].recordGauge(
        "plasma_node_cold_peers_count",
        "Number of peers in cold state for given node.",
        Map("host_id" -> stringToJson(show"${state.thisHostId}")),
        longToJson(coldPeers.size)
      )
    ) >>
    Async[F].defer(
      Stats[F].incrementCounter(
        "plasma_node_peer_id",
        "Counter with the sole purpose of adding the host attribute to track the peer id.",
        Map("host_id" -> stringToJson(show"${state.thisHostId}"))
      )
    ) >>
    state.peersHandler.forPeersWithActor(_.sendNoWait(PeerActor.Message.PrintCommonAncestor)).sequence >>
    (state, state).pure[F]
  }

  private def printQueueSizeInfo[F[_]: Async: Logger](thisActor: PeersManagerActor[F], state: State[F]): F[Unit] =
    for {
      blockCheckerSize  <- state.blocksChecker.traverse(_.mailboxSize())
      _                 <- Logger[F].info(show"BlockChecker queue size: $blockCheckerSize")
      thisActorSize     <- thisActor.mailboxSize()
      _                 <- Logger[F].info(show"PeersManager queue size: ${thisActorSize.some}")
      requestsProxySize <- state.requestsProxy.traverse(_.mailboxSize())
      _                 <- Logger[F].info(show"Requests proxy queue size: $requestsProxySize")
    } yield ()

  private def getCurrentTips[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info(show"Got request to get all available tips") >>
    state.peersHandler.getHotPeers.values.toSeq.traverse(_.sendNoWait(PeerActor.Message.GetCurrentTip)) >>
    (state, state).pure[F]

  private def remoteNetworkLevel[F[_]: Async: Logger](
    thisActor:          PeersManagerActor[F],
    state:              State[F],
    hostId:             HostId,
    networkLevelStatus: Boolean
  ): F[(State[F], Response[F])] =
    for {
      _        <- Logger[F].info(show"Update remote network level for peer $hostId to $networkLevelStatus")
      newPeers <- state.peersHandler.copyWithNetworkLevel(Set(hostId), networkLevelStatus, peerReleaseAction(thisActor))
      newState <- state.copy(peersHandler = newPeers).pure[F]
    } yield (newState, newState)

  private def updateExternalHotPeersList[F[_]: Async: Parallel: Logger](
    state: State[F]
  )(implicit res: ReverseDnsResolverHT[RemotePeer, F]): F[Unit] =
    for {
      hotPeersServers <- state.peersHandler.getHotPeers.values.flatMap(_.asServer).pure[F]
      _               <- Logger[F].debug(show"Resolve ip(s) to hostnames for hot peers ${hotPeersServers.toList}")
      hotPeersAsHosts <- hotPeersServers.toSeq.parTraverse(_.reverseResolving())
      _               <- Logger[F].debug(show"No filtered hot peers hostnames $hotPeersAsHosts")
      filteredHosts   <- hotPeersAsHosts.filter(state.peerFilter.remotePeerIsAcceptable).pure[F]
      _               <- Logger[F].debug(show"Filtered hot peers hostnames $filteredHosts")
      _               <- state.hotPeersUpdate(filteredHosts.toSet)
    } yield ()

  private def remoteIdChanged[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    oldId:     HostId,
    newId:     HostId
  ): F[(State[F], Response[F])] =
    // TODO move to COLD state?
    for {
      _               <- Logger[F].info(show"Try to change peer $oldId to $newId")
      newPeersHandler <- state.peersHandler.copyWithUpdatedId(oldId, newId, peerReleaseAction(thisActor))
      newState        <- state.copy(peersHandler = newPeersHandler).pure[F]
    } yield (newState, newState)

  private def coldPeer[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostIds:   NonEmptyChain[HostId]
  ): F[(State[F], Response[F])] =
    stopPeerActivity(thisActor, state, hostIds.toList.toSet, PeerState.Cold).map(newState => (newState, newState))

  // Disable application level AND close connection as well for peer
  private def closePeer[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostId:    HostId
  ): F[(State[F], Response[F])] =
    closePeers(thisActor, state, Set(hostId), PeerState.Cold).map(newState => (newState, newState))

  // Disable application level AND close connection as well for peers
  private def closePeers[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostIds:   Set[HostId],
    endState:  PeerState
  ): F[State[F]] = {
    require(!endState.isActive)
    for {
      _        <- Logger[F].info(show"Going to clean-up peer actor for $hostIds due to closed connection")
      newState <- stopPeerActivity(thisActor, state, hostIds, endState)
      peers <- newState.peersHandler.copyWithNetworkLevel(
        hostIds,
        netLevel = false,
        peerReleaseAction(thisActor)
      )
    } yield newState.copy(peersHandler = peers)
  }

  private def stopPeerActivity[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostIds:   Set[HostId],
    endStatus: PeerState
  ): F[State[F]] =
    if (hostIds.nonEmpty) {
      require(!endStatus.isActive)

      for {
        _              <- Logger[F].info(show"Going to stop network and application level for peers $hostIds")
        newPeerHandler <- state.peersHandler.moveToState(hostIds, endStatus, peerReleaseAction(thisActor))
        newState       <- state.copy(peersHandler = newPeerHandler).pure[F]
      } yield newState
    } else {
      state.pure[F]
    }

  private def banPeer[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostId:    HostId
  ): F[(State[F], Response[F])] =
    for {
      _        <- Logger[F].info(show"Going to ban peer $hostId")
      newState <- stopPeerActivity(thisActor, state, Set(hostId), PeerState.Banned)
    } yield (newState, newState)

  private def addLocalAddress[F[_]: Async: Logger](
    state:            State[F],
    localPeerAddress: RemoteAddress
  ): F[(State[F], Response[F])] =
    for {
      _ <- Logger[F].info(show"Added ${localPeerAddress.host} as known local address")
      newState = state.copy(thisHostIps = state.thisHostIps + localPeerAddress.host)
    } yield (newState, newState)

  private def openedPeerConnection[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    setupPeer: OpenedPeerConnection[F]
  ): F[(State[F], Response[F])] = {
    for {
      connectedPeer <- OptionT.liftF(setupPeer.client.remotePeer.pure[F])
      client        <- OptionT.some[F](setupPeer.client)
      hostId        <- OptionT.some[F](HostId(connectedPeer.p2pVK))
      _ <- OptionT
        .when[F, Unit](state.thisHostId != hostId)(())
        .flatTapNone(
          Logger[F].info(show"Decline connection ${connectedPeer.toString} because it is self-connection") >>
          client.closeConnection()
        )
      _ <- OptionT
        .when[F, Unit](state.peersHandler.hostIsNotBanned(hostId))(())
        .flatTapNone(
          Logger[F].warn(show"Actor for $hostId was not created because peer is banned. Connection will be closed") >>
          client.closeConnection()
        )
      _ <- OptionT
        .when[F, Unit](state.peersHandler.haveNoActorForHost(hostId))(())
        .flatTapNone(
          Logger[F].error(show"Try to open connection to already opened peer $hostId. Connection will be closed") >>
          client.closeConnection()
        )
      newState <- OptionT.liftF(updateStateByNewConnection(hostId, state, setupPeer, thisActor))
    } yield (newState, newState)
  }.getOrElse((state, state))

  private def updateStateByNewConnection[F[_]: Async: Logger](
    hostId:    HostId,
    state:     State[F],
    setupPeer: OpenedPeerConnection[F],
    thisActor: PeersManagerActor[F]
  ): F[State[F]] = {
    require(state.requestsProxy.isDefined)
    val client: BlockchainPeerClient[F] = setupPeer.client
    val remotePeer: Option[KnownHost] = setupPeer.remotePeerAsServer
    val connectedPeer: ConnectedPeer = setupPeer.client.remotePeer

    def setupPeerActor: F[PeerActor[F]] =
      thisActor
        .acquireActor(() =>
          state.networkAlgebra.makePeer(
            hostId,
            state.networkAlgebra,
            client,
            state.requestsProxy.get,
            thisActor,
            state.localChain,
            state.chainSelection,
            state.slotDataStore,
            state.bodyStore,
            state.transactionStore,
            state.headerToBodyValidation,
            state.transactionSyntaxValidation,
            state.mempool,
            commonAncestor,
            state.ed25519VRF,
            state.blockIdTree
          )
        )

    for {
      _ <- Logger[F].info(show"Going to create actor for handling connection to remote peer $hostId")

      peerActor <- setupPeerActor
      remotePeerAsServer = remotePeer.map(kh => RemotePeer(HostId(kh.id), RemoteAddress(kh.host, kh.port)))
      _ <- Logger[F].info(show"Received remote peer as server $remotePeerAsServer from $hostId")

      timeNow = System.currentTimeMillis()
      newPeerHandler <-
        state.peersHandler
          .copyWithUpdatedPeer(hostId, connectedPeer.remoteAddress, remotePeerAsServer, peerActor, timeNow)
          .pure[F]

      peerState = newPeerHandler.get(hostId).get.state
      _ <- peerActor.sendNoWait(PeerActor.Message.UpdateState(peerState.networkLevel, peerState.applicationLevel))
      _ <- peerActor.sendNoWait(PeerActor.Message.GetNetworkQuality)
    } yield state.copy(peersHandler = newPeerHandler)
  }

  private def resolveHosts[T: Show, F[_]: Async: Parallel: Logger](
    unresolved: Seq[T]
  )(implicit res: DnsResolverHT[T, F]): F[Seq[T]] =
    unresolved
      .parTraverse { unresolvedAddress =>
        unresolvedAddress
          .resolving()
          .flatTap(resolvedAddress => Logger[F].debug(show"Resolve address $unresolvedAddress to $resolvedAddress"))
      }
      .map(_.flatten.toSeq)

  private def addKnownNeighbors[F[_]: Async: Parallel: Logger: DnsResolver](
    state:       State[F],
    source:      HostId,
    remotePeers: NonEmptyChain[RemotePeer]
  ): F[(State[F], Response[F])] = {
    for {
      resolved        <- OptionT(resolveHosts(remotePeers.toList).map(NonEmptyChain.fromSeq))
      nonSpecialHosts <- OptionT.fromOption[F](NonEmptyChain.fromChain(resolved.filterNot(_.address.isSpecialHost)))
      neighbourBlockRep = state.peersHandler.get(source).fold(0.0)(_.blockRep)
      peerToAdd = nonSpecialHosts.map(rp => KnownRemotePeer(rp.peerId, rp.address, neighbourBlockRep, 0.0, None))
    } yield addKnownResolvedPeers(state, peerToAdd)
  }.getOrElse((state, state).pure[F]).flatten

  private def addKnownPeers[F[_]: Async: Parallel: Logger: DnsResolver](
    state:      State[F],
    knownPeers: NonEmptyChain[KnownRemotePeer]
  ): F[(State[F], Response[F])] = {
    for {
      resolvedPeers <- OptionT(resolveHosts(knownPeers.toList).map(NonEmptyChain.fromSeq))
      _             <- OptionT.liftF(Logger[F].info(show"Resolve $knownPeers to $resolvedPeers"))
    } yield addKnownResolvedPeers(state, resolvedPeers)
  }.getOrElse((state, state).pure[F]).flatten

  private def addKnownResolvedPeers[F[_]: Async: Logger](
    state:      State[F],
    knownPeers: NonEmptyChain[KnownRemotePeer]
  ): F[(State[F], Response[F])] =
    for {
      filteredLoopback <- knownPeers.filter(_.peerId != state.thisHostId).pure[F]
      oldPeers         <- state.peersHandler.pure[F]
      newPeers         <- oldPeers.copyWithAddedPeers(filteredLoopback).pure[F]
      changedPeers     <- (newPeers.peers -- oldPeers.peers.keySet).pure[F]
      _                <- Logger[F].infoIf(changedPeers.nonEmpty, show"New known peers: $changedPeers")
      newState         <- state.copy(peersHandler = newPeers).pure[F]
    } yield (newState, newState)

  private def forgetPeer[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    peer:      HostId
  ): F[(State[F], Response[F])] =
    for {
      (stateWithClosed, _) <- closePeer(thisActor, state, peer)
      newPeersHandler = stateWithClosed.peersHandler.removeColdPeers(Set(peer))
      newState = stateWithClosed.copy(peersHandler = newPeersHandler)
    } yield (newState, newState)

  private def updatePeersTick[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    for {
      _                          <- requestNeighboursFromHotPeers(state)
      stateWithUpdatedTimestamps <- updatePeersTimestamps(state).pure[F]
      newState                   <- clearColdPeers(stateWithUpdatedTimestamps).pure[F]
    } yield (newState, newState)

  private def requestNeighboursFromHotPeers[F[_]: Async: Logger](state: State[F]): F[Unit] = {
    val newWarmPeerCount = state.p2pNetworkConfig.networkProperties.maximumWarmConnections
    val warmPeersSize = state.peersHandler.getWarmPeersWithActor.size

    if (warmPeersSize < newWarmPeerCount && newWarmPeerCount > 0) {
      val currentHotPeers = state.peersHandler.getHotPeers
      Logger[F].debug(show"Request $newWarmPeerCount neighbour(s) from peers: ${currentHotPeers.keySet}") >>
      currentHotPeers.values.toList
        .traverse(_.sendNoWait(PeerActor.Message.GetHotPeersFromPeer(newWarmPeerCount)))
        .void
    } else {
      Logger[F].info(
        show"Do not request neighbours, warmPeersSize: $warmPeersSize, newWarmPeerCount: $newWarmPeerCount"
      )
    }
  }

  private def clearColdPeers[F[_]](state: State[F]): State[F] = {
    val minimumColdConnections = state.p2pNetworkConfig.networkProperties.minimumEligibleColdConnections
    val maximumColdConnections = state.p2pNetworkConfig.networkProperties.maximumEligibleColdConnections
    val clearColdIfNotActiveForInMs = state.p2pNetworkConfig.networkProperties.clearColdIfNotActiveForInMs

    val timeNow = System.currentTimeMillis()
    val openedAfter = timeNow - clearColdIfNotActiveForInMs
    val usefulColdConnections =
      state.peersHandler.getColdPeers
        .filter(_._2.isUseful)
        .filter { case (_, peer) => peer.activeConnectionTimestamp.getOrElse(timeNow) > openedAfter || peer.isActive }

    val coldConnectionsToSave =
      if (usefulColdConnections.size > maximumColdConnections) {
        val (activeCold, noActiveCold) = usefulColdConnections.toSeq.partition(_._2.isActive)
        val sortedNoActiveCold = noActiveCold.sortBy(_._2.closedTimestamps.size)
        (activeCold ++ sortedNoActiveCold).take(minimumColdConnections).toMap
      } else {
        usefulColdConnections
      }

    val toRemove = state.peersHandler.getColdPeers.keySet -- coldConnectionsToSave.keySet
    val newPeer = state.peersHandler.removeColdPeers(toRemove)
    state.copy(peersHandler = newPeer)
  }

  private def updatePeersTimestamps[F[_]](state: State[F]): State[F] =
    state.copy(peersHandler = state.peersHandler.copyWithUpdatedTimestamps)

  private def aggressiveP2PUpdate[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F]
  ): F[(State[F], Response[F])] = {
    val availableWarm = state.peersHandler.getWarmPeersWithActor.filter { case (id, _) =>
      state.peersHandler
        .peers(id)
        .closedTimestamps
        .sizeIs < state.p2pNetworkConfig.networkProperties.aggressiveP2PMaxCloseEvent
    }
    val toHot = state.warmToHotSelector
      .select(
        availableWarm,
        state.p2pNetworkConfig.networkProperties.aggressiveP2PCount
      )
    warmPeersToHot(thisActor, state, toHot).map(s => (s, s))
  }

  private def repUpdate[F[_]: Async: Parallel: Logger: DnsResolver: ReverseDnsResolver](
    thisActor: PeersManagerActor[F],
    state:     State[F]
  ): F[(State[F], Response[F])] =
    for {
      _                    <- Logger[F].debug(show"Peers: ${state.peersHandler.peers.values.toList}")
      stateWithLastRep     <- getPeerHandlerAfterReputationDecoy(state).pure[F]
      stateWithPreWarm     <- coldToWarm(thisActor, stateWithLastRep)
      stateWithClosedPeers <- hotToCold(thisActor, stateWithPreWarm)
      stateWithNewHotPeers <- warmToHot(thisActor, stateWithClosedPeers)
      _                    <- updateExternalHotPeersList(stateWithNewHotPeers)
      _ <- stateWithNewHotPeers.peersHandler.getHotPeers.values.toList
        .traverse(_.sendNoWait(PeerActor.Message.ReputationUpdateTick))
    } yield (stateWithNewHotPeers, stateWithNewHotPeers)

  private def getPeerHandlerAfterReputationDecoy[F[_]](
    state: State[F]
  ): State[F] = {
    val blockNoveltyDecoy = state.p2pNetworkConfig.blockNoveltyDecoy

    val blockRepMap =
      state.peersHandler.getHotPeers.map { case (id, peer) => id -> peer.blockRep * blockNoveltyDecoy }

    val noveltyRepMap =
      state.peersHandler.getHotPeers.map { case (id, peer) => id -> Math.max(peer.newRep - 1, 0) }

    val newPeersHandler =
      state.peersHandler.copyWithUpdatedReputation(blockRepMap = blockRepMap, noveltyRepMap = noveltyRepMap)

    state.copy(peersHandler = newPeersHandler)
  }

  private def hotToCold[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F]
  ): F[State[F]] = {
    val currentHotPeers: Map[HostId, Peer[F]] = state.peersHandler.getHotPeers
    val hotToCold = getHostsToCold(currentHotPeers, state.p2pNetworkConfig)

    for {
      _        <- Logger[F].infoIf(hotToCold.nonEmpty, show"Going to cold $hotToCold due of bad reputation")
      newState <- stopPeerActivity(thisActor, state, hotToCold, PeerState.Cold)
    } yield newState
  }

  private def warmToHot[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F]
  ): F[State[F]] = {
    val minimumHotConnections = state.p2pNetworkConfig.networkProperties.minimumHotConnections

    val lackHotPeersCount =
      minimumHotConnections - state.peersHandler.getHotPeers.count(_._2.asServer.isDefined)

    val warmToHot =
      state.warmToHotSelector.select(state.peersHandler.getWarmPeersWithActor, lackHotPeersCount)

    warmPeersToHot(thisActor, state, warmToHot)
  }

  private def getEligibleColdPeers[F[_]](state: State[F]): Map[HostId, Peer[F]] = {
    val closeTimeoutFirstDelayInMs = state.p2pNetworkConfig.networkProperties.closeTimeoutFirstDelayInMs
    val currentTimestamp = System.currentTimeMillis()

    state.peersHandler.getColdPeers.filter { case (_, host) =>
      val timestamps = host.closedTimestamps
      val lastClose = timestamps.lastOption.getOrElse(0L)
      val totalCloses = timestamps.size
      val nonEligibleWindow = totalCloses * totalCloses * closeTimeoutFirstDelayInMs
      currentTimestamp.toDouble >= (lastClose + nonEligibleWindow)
    }
  }

  private def coldToWarm[F[_]: Async: Parallel: Logger: DnsResolver](
    thisActor: PeersManagerActor[F],
    state:     State[F]
  ): F[State[F]] = {
    val maximumWarmConnection = state.p2pNetworkConfig.networkProperties.maximumWarmConnections
    val lackWarmPeersCount = maximumWarmConnection - state.peersHandler.getWarmPeersWithActor.size

    for {
      coldPeers    <- getEligibleColdPeers(state).filter(_._2.couldOpenConnection).pure[F]
      coldToWarm   <- state.coldToWarmSelector.select(coldPeers, lackWarmPeersCount).pure[F]
      _            <- Logger[F].infoIf(coldToWarm.nonEmpty, show"Going to warm next cold peers: $coldToWarm")
      peersHandler <- state.peersHandler.moveToState(coldToWarm, PeerState.Warm, peerReleaseAction(thisActor))
      newState     <- state.copy(peersHandler = peersHandler).pure[F]
      _            <- checkConnection(newState, coldToWarm)
    } yield newState
  }

  private def checkConnection[F[_]: Async: Parallel: Logger: DnsResolver](
    state:        State[F],
    hostsToCheck: Set[HostId]
  ): F[Unit] = {
    val addressesToOpen: Seq[RemotePeer] =
      hostsToCheck
        .flatMap(id => state.peersHandler.get(id))
        .filter(p => p.haveNoConnection && p.state.isActive && p.asServer.isDefined)
        .flatMap(_.asServer)
        .toSeq

    for {
      resolved <- resolveHosts(addressesToOpen)
      _        <- Logger[F].infoIf(resolved.nonEmpty, show"Going to open connection to next peers: $resolved")
      toOpen   <- resolved.map(rp => DisconnectedPeer(rp.address, rp.peerId.id.some)).pure[F]
      _        <- toOpen.traverse(state.newPeerCreationAlgebra.requestNewPeerCreation)
    } yield ()
  }

  private def getHostsToCold[F[_]](
    currentHotPeers:  Map[HostId, Peer[F]],
    p2pNetworkConfig: P2PNetworkConfig
  ): Set[HostId] = {
    val minBlockRep = p2pNetworkConfig.networkProperties.minimumBlockProvidingReputation

    val saveByPerformanceReputation =
      currentHotPeers.toSeq
        .filter(_._2.blockRep >= minBlockRep)
        .map { case (id, peer) => id -> peer.perfRep }
        .sortBy(_._2)
        .takeRight(p2pNetworkConfig.networkProperties.minimumPerformanceReputationPeers)
        .map(_._1)
        .toSet

    val saveByBlockProviding =
      currentHotPeers.toSeq
        .map { case (id, peer) => id -> peer.blockRep }
        .filter(_._2 > 0)
        .sortBy(_._2)
        .takeRight(p2pNetworkConfig.networkProperties.minimumBlockProvidingReputationPeers)
        .map(_._1)
        .toSet

    val saveByNovelty = currentHotPeers
      .map { case (id, peer) => id -> peer.newRep }
      .filter(_._2 > 0)
      .keySet

    val saveByOverallReputation =
      currentHotPeers
        .filter(_._2.blockRep >= minBlockRep)
        .filter { case (_, peer) =>
          val totalRep = peer.reputation
          totalRep >= p2pNetworkConfig.networkProperties.minimumRequiredReputation
        }
        .keySet

    val saveByMempoolTxProviding =
      currentHotPeers
        .filter(_._2.mempoolTxRep > (1 / p2pNetworkConfig.networkProperties.txImpactRatio))
        .toList
        .sortBy(_._2.mempoolTxRep)
        .takeRight(p2pNetworkConfig.networkProperties.minimumTxMempoolReputationPeers)
        .map(_._1)
        .toSet

    val allKeptConnections =
      saveByNovelty ++ saveByBlockProviding ++ saveByPerformanceReputation ++
      saveByOverallReputation ++ saveByMempoolTxProviding

    currentHotPeers.keySet -- allKeptConnections
  }

  private def warmPeersToHot[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    toHot:     Set[HostId]
  ): F[State[F]] =
    for {
      _            <- Logger[F].infoIf(toHot.nonEmpty, show"Going to hot next hosts: $toHot")
      updatedPeers <- state.peersHandler.moveToState(toHot, PeerState.Hot, peerReleaseAction(thisActor))
    } yield state.copy(peersHandler = updatedPeers)

}
// scalastyle:on number.of.methods
