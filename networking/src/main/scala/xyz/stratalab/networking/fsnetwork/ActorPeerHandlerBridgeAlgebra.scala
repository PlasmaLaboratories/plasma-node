package xyz.stratalab.networking.fsnetwork

import cats.effect.implicits._
import cats.effect.kernel.Sync
import cats.effect.{Async, Deferred, Resource}
import cats.implicits._
import cats.{Monad, MonadThrow}
import fs2.concurrent.Topic
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import xyz.stratalab.algebras.Stats
import xyz.stratalab.blockchain.BlockchainCore
import xyz.stratalab.config.ApplicationConfig.Node.NetworkProperties
import xyz.stratalab.consensus.models.{BlockHeader, BlockId, SlotData}
import xyz.stratalab.crypto.signing.Ed25519VRF
import xyz.stratalab.models.p2p._
import xyz.stratalab.models.utility.NetworkCommands
import xyz.stratalab.networking.blockchain.{BlockchainPeerClient, BlockchainPeerHandlerAlgebra}
import xyz.stratalab.networking.fsnetwork.P2PShowInstances._
import xyz.stratalab.networking.fsnetwork.PeersManager.PeersManagerActor
import xyz.stratalab.networking.p2p.{ConnectedPeer, DisconnectedPeer, PeerConnectionChange}
import xyz.stratalab.node.models._
import xyz.stratalab.sdk.models.TransactionId
import xyz.stratalab.sdk.models.transaction.IoTransaction
import xyz.stratalab.typeclasses.implicits._

object ActorPeerHandlerBridgeAlgebra {

  // scalastyle:off parameter.number
  def make[F[_]: Async: DnsResolver: ReverseDnsResolver: Stats](
    thisHostId:              HostId,
    blockchain:              BlockchainCore[F],
    networkProperties:       NetworkProperties,
    remotePeers:             Seq[DisconnectedPeer],
    peersStatusChangesTopic: Topic[F, PeerConnectionChange],
    addRemotePeer:           DisconnectedPeer => F[Unit],
    hotPeersUpdate:          Set[RemotePeer] => F[Unit],
    ed25519VRF:              Resource[F, Ed25519VRF],
    networkCommands:         Topic[F, NetworkCommands]
  ): Resource[F, BlockchainPeerHandlerAlgebra[F]] = {
    implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName("Node.P2P")

    val networkAlgebra = new NetworkAlgebraImpl[F](blockchain.clock)
    val networkManager =
      NetworkManager.startNetwork[F](
        thisHostId,
        blockchain,
        networkAlgebra,
        remotePeers,
        networkProperties,
        PeerCreationRequestAlgebra(addRemotePeer),
        peersStatusChangesTopic,
        hotPeersUpdate,
        ed25519VRF,
        networkCommands
      )

    networkManager.map(pm => makeAlgebra(pm))
  }
  // scalastyle:on parameter.number

  private def makeAlgebra[F[_]: Async: Logger](peersManager: PeersManagerActor[F]): BlockchainPeerHandlerAlgebra[F] = {
    underlyingClient: BlockchainPeerClient[F] =>
      for {
        // A callback/Deferred that is signaled by the "closeConnection()" call
        closeDeferred <- Deferred[F, Unit].toResource
        client = new BlockchainPeerClientCloseHook[F](underlyingClient, Async[F].defer(closeDeferred.complete(()).void))
        remoteId <- client.remotePeer.p2pVK.pure[F].toResource
        remotePeerOpt <- client.remotePeerAsServer.handleErrorWith { e =>
          Logger[F].error(show"Failed to get remote peer as server from $remoteId due ${e.toString}") >>
          Option.empty[KnownHost].pure[F]
        }.toResource

        peerAsServer = remotePeerOpt match {
          case Some(kh) if kh.id == remoteId => kh.some
          case _                             => None
        }

        _ <-
          if (remotePeerOpt.isDefined && peerAsServer.isEmpty)
            Logger[F].warn(show"Remote peer $remoteId provide bad server info $remotePeerOpt").toResource
          else Resource.pure[F, Unit](())
        _ <- peersManager.sendNoWait(PeersManager.Message.OpenedPeerConnection(client, peerAsServer)).toResource
        _ <- closeDeferred.get.toResource
        _ <- Logger[F].info(show"Remote peer $remoteId complete").toResource
      } yield ()
  }
}

/**
 * Wraps a BlockchainPeerClient instance with an extra hook on the "closeConnection" method
 * @param underlying an underlying client instance
 * @param onClose a hook to be invoked when closeConnection is called
 */
class BlockchainPeerClientCloseHook[F[_]: Monad](underlying: BlockchainPeerClient[F], onClose: F[Unit])
    extends BlockchainPeerClient[F] {

  override def remotePeer: ConnectedPeer = underlying.remotePeer

  override def remotePeerAsServer: F[Option[KnownHost]] = underlying.remotePeerAsServer

  override def remotePeerAdoptions: F[fs2.Stream[F, BlockId]] = underlying.remotePeerAdoptions

  override def remoteTransactionNotifications: F[fs2.Stream[F, TransactionId]] =
    underlying.remoteTransactionNotifications

  override def getRemoteBlockIdAtDepth(depth: Long): F[Option[BlockId]] = underlying.getRemoteBlockIdAtDepth(depth)

  override def getRemoteSlotData(id: BlockId): F[Option[SlotData]] = underlying.getRemoteSlotData(id)

  override def getRemoteSlotDataWithParents(from: BlockId, to: BlockId): F[Option[List[SlotData]]] =
    underlying.getRemoteSlotDataWithParents(from, to)

  override def getRemoteHeader(id: BlockId): F[Option[BlockHeader]] = underlying.getRemoteHeader(id)

  override def getRemoteBody(id: BlockId): F[Option[BlockBody]] = underlying.getRemoteBody(id)

  override def getRemoteTransaction(id: TransactionId): F[Option[IoTransaction]] = underlying.getRemoteTransaction(id)

  override def getRemoteBlockIdAtHeight(height: Long): F[Option[BlockId]] = underlying.getRemoteBlockIdAtHeight(height)

  override def getRemoteKnownHosts(request: CurrentKnownHostsReq): F[Option[CurrentKnownHostsRes]] =
    underlying.getRemoteKnownHosts(request)

  override def getPongMessage(request: PingMessage): F[Option[PongMessage]] = underlying.getPongMessage(request)

  override def notifyAboutThisNetworkLevel(networkLevel: Boolean): F[Unit] =
    underlying.notifyAboutThisNetworkLevel(networkLevel)

  override def closeConnection(): F[Unit] = onClose >> underlying.closeConnection()

  override def getRemoteTransactionOrError[E <: Throwable](id: TransactionId, error: => E)(implicit
    MonadThrow: MonadThrow[F]
  ): F[IoTransaction] = underlying.getRemoteTransactionOrError(id, error)

  override def findCommonAncestor(
    getLocalBlockIdAtHeight: Long => F[BlockId],
    currentHeight:           () => F[Long]
  )(implicit syncF: Sync[F], loggerF: Logger[F]): F[BlockId] =
    underlying.findCommonAncestor(getLocalBlockIdAtHeight, currentHeight)

}
