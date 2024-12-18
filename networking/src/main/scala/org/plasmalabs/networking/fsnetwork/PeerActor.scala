package org.plasmalabs.networking.fsnetwork

import cats.data.{EitherT, NonEmptyChain, OptionT}
import cats.effect.implicits.*
import cats.effect.{Async, Concurrent, Resource}
import cats.implicits.*
import org.plasmalabs.actor.{Actor, Fsm}
import org.plasmalabs.algebras.Store
import org.plasmalabs.consensus.algebras.{BlockHeaderToBodyValidationAlgebra, ChainSelectionAlgebra, LocalChainAlgebra}
import org.plasmalabs.consensus.models.{BlockHeader, BlockId, SlotData}
import org.plasmalabs.crypto.signing.Ed25519VRF
import org.plasmalabs.eventtree.ParentChildTree
import org.plasmalabs.ledger.algebras.MempoolAlgebra
import org.plasmalabs.models.p2p.*
import org.plasmalabs.networking.KnownHostOps
import org.plasmalabs.networking.blockchain.BlockchainPeerClient
import org.plasmalabs.networking.fsnetwork.P2PShowInstances.*
import org.plasmalabs.networking.fsnetwork.PeerActor.Message.*
import org.plasmalabs.networking.fsnetwork.PeerBlockBodyFetcher.PeerBlockBodyFetcherActor
import org.plasmalabs.networking.fsnetwork.PeerBlockHeaderFetcher.PeerBlockHeaderFetcherActor
import org.plasmalabs.networking.fsnetwork.PeerMempoolTransactionSync.PeerMempoolTransactionSyncActor
import org.plasmalabs.networking.fsnetwork.PeersManager.PeersManagerActor
import org.plasmalabs.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import org.plasmalabs.node.models.{BlockBody, CurrentKnownHostsReq, PingMessage}
import org.plasmalabs.sdk.models.TransactionId
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.validation.algebras.TransactionSyntaxVerifier
import org.plasmalabs.typeclasses.implicits.*
import org.typelevel.log4cats.Logger

import scala.util.Random

object PeerActor {
  sealed trait Message

  object Message {

    /**
     * Update peer state
     * @param networkLevel do we run network level data exchange like measuring metwork quality
     * @param applicationLevel do we run application level data exchange
     */
    case class UpdateState(networkLevel: Boolean, applicationLevel: Boolean) extends Message

    /**
     * Close connection on client side
     */
    case object CloseConnectionForActor extends Message

    /**
     * Request to download block headers from peer, downloaded headers will be sent to block checker directly
     * @param blockIds headers block id to download
     */
    case class DownloadBlockHeaders(blockIds: NonEmptyChain[BlockId]) extends Message

    /**
     * Request to download block bodies from peer, downloaded bodies will be sent to block checker directly
     * @param blockData bodies block id to download
     */
    case class DownloadBlockBodies(blockData: NonEmptyChain[BlockHeader]) extends Message

    /**
     * Request current tip from remote peer
     */
    case object GetCurrentTip extends Message

    /**
     * Request network quality measure
     */
    case object GetNetworkQuality extends Message

    /**
     * Find and print common ancestor for remote peer
     */
    case object PrintCommonAncestor extends Message

    /**
     * Request to get remote host's hot connections
     */
    case class GetHotPeersFromPeer(maxHosts: Int) extends Message

    /**
     * Request reputation update
     */
    case object ReputationUpdateTick extends Message
  }

  case class State[F[_]](
    hostId:           HostId,
    client:           BlockchainPeerClient[F],
    blockHeaderActor: PeerBlockHeaderFetcherActor[F],
    blockBodyActor:   PeerBlockBodyFetcherActor[F],
    mempoolSync:      PeerMempoolTransactionSyncActor[F],
    peersManager:     PeersManagerActor[F],
    localChain:       LocalChainAlgebra[F],
    chainSelection:   ChainSelectionAlgebra[F, BlockId, SlotData],
    slotDataStore:    Store[F, BlockId, SlotData],
    networkLevel:     Boolean,
    applicationLevel: Boolean,
    genesisBlockId:   BlockId,
    commonAncestorF:  CommonAncestorF[F]
  )

  type Response[F[_]] = State[F]
  type PeerActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] = Fsm {
    case (state, UpdateState(networkLevel, applicationLevel)) => updateState(state, networkLevel, applicationLevel)
    case (state, CloseConnectionForActor)                     => closeConnection(state)
    case (state, DownloadBlockHeaders(blockIds))              => downloadHeaders(state, blockIds)
    case (state, DownloadBlockBodies(blockHeaders))           => downloadBodies(state, blockHeaders)
    case (state, GetCurrentTip)                               => getCurrentTip(state)
    case (state, GetNetworkQuality)                           => getNetworkQuality(state)
    case (state, PrintCommonAncestor)                         => printCommonAncestor(state)
    case (state, GetHotPeersFromPeer(maxHosts))               => getHotPeersOfCurrentPeer(state, maxHosts)
    case (state, ReputationUpdateTick)                        => reputationUpdateTick(state)
  }

  // scalastyle:off parameter.number
  def makeActor[F[_]: Async: Logger](
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
  ): Resource[F, PeerActor[F]] = {
    val initNetworkLevel = false
    val initAppLevel = false

    for {
      actorName <- Resource.pure(show"Peer Actor for peer $hostId")
      _         <- Resource.onFinalize(Logger[F].info(show"$actorName: is released") >> client.closeConnection())

      header <- networkAlgebra.makePeerHeaderFetcher(
        hostId,
        client,
        requestsProxy,
        peersManager,
        localChain,
        chainSelection,
        slotDataStore,
        bodyStore,
        commonAncestorF,
        ed25519VRF,
        blockIdTree
      )
      body <- networkAlgebra.makePeerBodyFetcher(
        hostId,
        client,
        requestsProxy,
        transactionStore,
        headerToBodyValidation,
        transactionSyntaxValidation
      )

      mempoolSync <- networkAlgebra.makeMempoolSyncFetcher(
        hostId,
        client,
        transactionSyntaxValidation,
        transactionStore,
        mempool,
        peersManager,
        localChain
      )

      genesisSlotData <- localChain.genesis.toResource
      initialState = State(
        hostId,
        client,
        header,
        body,
        mempoolSync,
        peersManager,
        localChain,
        chainSelection,
        slotDataStore,
        initNetworkLevel,
        initAppLevel,
        genesisSlotData.slotId.blockId,
        commonAncestorF
      )
      _     <- verifyGenesisAgreement(initialState).toResource
      actor <- Actor.makeWithFinalize(actorName, initialState, getFsm[F], finalizer[F])
    } yield actor
  }
  // scalastyle:on parameter.number

  private def finalizer[F[_]: Async: Logger](state: State[F]): F[Unit] =
    Logger[F].info(show"Run finalizer for actor for peer ${state.hostId}") >>
    state.client.notifyAboutThisNetworkLevel(false)

  private def updateState[F[_]: Async: Logger](
    state:               State[F],
    newNetworkLevel:     Boolean,
    newApplicationLevel: Boolean
  ): F[(State[F], Response[F])] = {
    val networkLevel: F[Unit] =
      (state.networkLevel != newNetworkLevel, newNetworkLevel) match {
        case (true, true)  => startNetworkLevel(state)
        case (true, false) => stopNetworkLevel(state)
        case (false, _)    => ().pure[F]
      }

    val applicationLevel: F[Unit] =
      (state.applicationLevel != newApplicationLevel, newApplicationLevel) match {
        case (true, true)  => startApplicationLevel(state)
        case (true, false) => stopApplicationLevel(state)
        case (false, _)    => ().pure[F]
      }

    val newState: State[F] = state.copy(applicationLevel = newApplicationLevel, networkLevel = newNetworkLevel)
    applicationLevel >> networkLevel >> (newState, newState).pure[F]
  }

  private def closeConnection[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info(show"Going to close connection ${state.hostId}") >>
    stopApplicationLevel(state) >>
    stopNetworkLevel(state) >>
    state.client.closeConnection() >>
    (state, state).pure[F]

  private def startApplicationLevel[F[_]: Async: Logger](state: State[F]): F[Unit] =
    Logger[F].info(show"Application level is enabled for ${state.hostId}") >>
    state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.StartActor) >>
    state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.StartActor) >>
    state.mempoolSync.sendNoWait(PeerMempoolTransactionSync.Message.StartActor)

  private def stopApplicationLevel[F[_]: Async: Logger](state: State[F]): F[Unit] =
    Logger[F].info(show"Application level is disabled for ${state.hostId}") >>
    state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.StopActor) >>
    state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.StopActor) >>
    state.mempoolSync.sendNoWait(PeerMempoolTransactionSync.Message.StopActor)

  private def startNetworkLevel[F[_]: Async: Logger](state: State[F]): F[Unit] =
    getNetworkQuality(state) >>
    state.client.notifyAboutThisNetworkLevel(networkLevel = true) >>
    Logger[F].info(show"Network level is started for ${state.hostId}")

  private def stopNetworkLevel[F[_]: Async: Logger](state: State[F]): F[Unit] =
    state.client.notifyAboutThisNetworkLevel(networkLevel = false) >>
    Logger[F].info(show"Network level is stop for ${state.hostId}")

  private def downloadHeaders[F[_]: Concurrent](
    state:    State[F],
    blockIds: NonEmptyChain[BlockId]
  ): F[(State[F], Response[F])] =
    state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(blockIds)) >>
    (state, state).pure[F]

  private def downloadBodies[F[_]: Concurrent](
    state:     State[F],
    blockData: NonEmptyChain[BlockHeader]
  ): F[(State[F], Response[F])] =
    state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.DownloadBlocks(blockData)) >>
    (state, state).pure[F]

  private def getCurrentTip[F[_]: Concurrent](state: State[F]): F[(State[F], Response[F])] =
    state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.GetCurrentTip) >>
    (state, state).pure[F]

  private def getHotPeersOfCurrentPeer[F[_]: Async: Logger](
    state:    State[F],
    maxHosts: Int
  ): F[(State[F], Response[F])] = {
    for {
      _        <- OptionT.liftF(Logger[F].debug(show"Request $maxHosts neighbour(s) from ${state.hostId}"))
      response <- OptionT(state.client.getRemoteKnownHosts(CurrentKnownHostsReq(maxHosts)))
      hotPeers <- OptionT
        .fromOption[F](NonEmptyChain.fromSeq(response.hotHosts.map(_.asRemotePeer)))
        .flatTapNone(Logger[F].info(show"Got no hot peers from ${state.hostId}"))
      _ <- OptionT.liftF(Logger[F].debug(show"Got hot peers $hotPeers from ${state.hostId}"))
      _ <- OptionT.liftF(state.peersManager.sendNoWait(PeersManager.Message.AddKnownNeighbors(state.hostId, hotPeers)))
    } yield (state, state)
  }.getOrElse((state, state))
    .handleErrorWith { error =>
      Logger[F].error(show"Error ${error.toString} during getting remote peer(s) neighbours") >>
      state.peersManager.sendNoWait(PeersManager.Message.NonCriticalErrorForHost(state.hostId)) >>
      (state, state).pure[F]
    }

  private def getNetworkQuality[F[_]: Concurrent: Logger](state: State[F]): F[(State[F], Response[F])] =
    getPing(state).value
      .flatMap { res =>
        val message =
          PeersManager.Message.PingPongMessagePing(state.hostId, res)
        Logger[F].debug(show"From host ${state.hostId}: $message") >>
        state.peersManager.sendNoWait(message)
      }
      .handleErrorWith { error =>
        Logger[F].error(show"Error ${error.toString} during getting remote peer network quality") >>
        state.peersManager.sendNoWait(PeersManager.Message.NonCriticalErrorForHost(state.hostId))
      } >> (state, state).pure[F]

  private def reputationUpdateTick[F[_]: Concurrent](state: State[F]): F[(State[F], Response[F])] =
    state.mempoolSync.sendNoWait(PeerMempoolTransactionSync.Message.CollectTransactionsRep) >> (state, state).pure[F]

  private val incorrectPongMessage: NetworkQualityError = NetworkQualityError.IncorrectPongMessage: NetworkQualityError

  // 1024 hardcoded on protobuf level, see org.plasmalabs.node.models.PingMessageValidator.validate
  private val pingMessageSize = 1024

  private def getPing[F[_]: Concurrent](state: State[F]): EitherT[F, NetworkQualityError, Long] =
    for {
      pingMessage <- EitherT.liftF(PingMessage(Random.nextString(pingMessageSize)).pure[F])
      before      <- EitherT.liftF(System.currentTimeMillis().pure[F])
      pongMessage <- EitherT.fromOptionF(state.client.getPongMessage(pingMessage), NetworkQualityError.NoPongMessage)
      after       <- EitherT.liftF(System.currentTimeMillis().pure[F])
      pongCorrect = pongMessage.pong.reverse == pingMessage.ping
      ping = after - before
      res <- EitherT.fromEither[F](Either.cond(pongCorrect, ping, incorrectPongMessage))
    } yield res

  /**
   * Ensure that the two peers agree on a genesis block.  If not, raise an exception.
   */
  private def verifyGenesisAgreement[F[_]: Async: Logger](state: State[F]): F[Unit] =
    state.client
      .getRemoteBlockIdAtHeight(1) // TODO replace 1 to BigBang.Height
      .flatMap(
        OptionT
          .fromOption[F](_)
          .getOrRaise(new IllegalStateException("Remote peer does not have a genesis block"))
          .flatMap(remoteGenesisId =>
            Async[F].raiseWhen(remoteGenesisId =!= state.genesisBlockId)(
              new IllegalStateException("Remote peer is using a different genesis block")
            )
          )
      )
      .recoverWith { case exception =>
        Logger[F].error(show"Remote peer ${state.hostId} failed to provide correct genesis: ${exception.toString}") >>
        state.peersManager.sendNoWait(PeersManager.Message.NonCriticalErrorForHost(state.hostId))
      }

  private def printCommonAncestor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    state
      .commonAncestorF(state.client, state.localChain)
      .flatTap(ancestor =>
        state.slotDataStore
          .getOrRaise(ancestor)
          .flatMap(slotData =>
            Logger[F].info(
              show"Traced common ancestor to" +
              show" peer=${state.hostId}" +
              show" id=$ancestor" +
              show" height=${slotData.height}" +
              show" slot=${slotData.slotId.slot}"
            )
          )
      )
      .void
      .handleErrorWith { e =>
        Logger[F].error(show"Common ancestor trace for peer ${state.hostId} is failed because ${e.toString}") >>
        state.peersManager.sendNoWait(PeersManager.Message.NonCriticalErrorForHost(state.hostId))
      } >> (state, state).pure[F]
}
