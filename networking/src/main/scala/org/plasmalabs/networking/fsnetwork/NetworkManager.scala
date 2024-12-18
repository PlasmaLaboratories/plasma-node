package org.plasmalabs.networking.fsnetwork

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.Async
import cats.effect.implicits.genSpawnOps
import cats.effect.kernel.{Outcome, Resource}
import cats.implicits.*
import com.google.protobuf.ByteString
import fs2.concurrent.Topic
import org.plasmalabs.algebras.Store
import org.plasmalabs.blockchain.BlockchainCore
import org.plasmalabs.config.ApplicationConfig.Node.NetworkProperties
import org.plasmalabs.crypto.signing.Ed25519VRF
import org.plasmalabs.models.p2p.*
import org.plasmalabs.models.utility.NetworkCommands
import org.plasmalabs.networking.fsnetwork.P2PShowInstances.*
import org.plasmalabs.networking.fsnetwork.PeersManager.PeersManagerActor
import org.plasmalabs.networking.p2p.{DisconnectedPeer, PeerConnectionChange, PeerConnectionChanges}
import org.typelevel.log4cats.Logger

import scala.util.Random

object NetworkManager {

  def startNetwork[F[_]: Async: Logger](
    thisHostId:              HostId,
    blockchain:              BlockchainCore[F],
    networkAlgebra:          NetworkAlgebra[F],
    initialHosts:            Seq[DisconnectedPeer],
    networkProperties:       NetworkProperties,
    addRemotePeerAlgebra:    PeerCreationRequestAlgebra[F],
    peersStatusChangesTopic: Topic[F, PeerConnectionChange],
    hotPeersUpdate:          Set[RemotePeer] => F[Unit],
    ed25519VRF:              Resource[F, Ed25519VRF],
    networkCommands:         Topic[F, NetworkCommands]
  ): Resource[F, PeersManagerActor[F]] =
    for {
      _ <- Resource.liftK(Logger[F].info(show"Start actors network with list of peers: ${initialHosts.mkString(";")}"))
      slotDuration <- Resource.liftK(blockchain.clock.slotLength)
      p2pNetworkConfig = P2PNetworkConfig(networkProperties, slotDuration)

      peersFromStorage <- Resource.liftK(blockchain.dataStores.knownHosts.get(()).map(_.getOrElse(Seq.empty)))
      _                <- Resource.liftK(Logger[F].info(show"Loaded from storage next known hosts: $peersFromStorage"))
      peerManager <- networkAlgebra.makePeerManger(
        thisHostId,
        networkAlgebra,
        blockchain.consensus.localChain,
        blockchain.consensus.chainSelection,
        blockchain.dataStores.slotData,
        blockchain.dataStores.bodies,
        blockchain.dataStores.transactions,
        blockchain.blockIdTree,
        blockchain.ledger.mempool,
        blockchain.validators.headerToBody,
        blockchain.validators.transactionSyntax,
        addRemotePeerAlgebra,
        p2pNetworkConfig,
        hotPeersUpdate,
        buildSaveRemotePeersFunction(blockchain.dataStores.knownHosts),
        ed25519VRF
      )

      requestsProxy <- networkAlgebra.makeRequestsProxy(
        peerManager,
        blockchain.dataStores.headers,
        blockchain.dataStores.bodies
      )
      blocksChecker <- networkAlgebra.makeBlockChecker(
        requestsProxy,
        blockchain.consensus.localChain,
        blockchain.dataStores.slotData,
        blockchain.dataStores.headers,
        blockchain.dataStores.bodies,
        blockchain.validators,
        blockchain.consensus.chainSelection,
        ed25519VRF,
        p2pNetworkConfig
      )

      _ <- Resource.liftK(requestsProxy.sendNoWait(RequestsProxy.Message.SetupBlockChecker(blocksChecker)))
      _ <- Resource.liftK(peerManager.sendNoWait(PeersManager.Message.SetupBlockChecker(blocksChecker)))
      _ <- Resource.liftK(peerManager.sendNoWait(PeersManager.Message.SetupRequestsProxy(requestsProxy)))

      _ <- Resource.liftK(
        NonEmptyChain
          .fromSeq(mergeRemotePeersAndRemoteAddress(peersFromStorage, initialHosts))
          .map { initialPeers =>
            peerManager.sendNoWait(PeersManager.Message.AddKnownPeers(initialPeers))
          }
          .getOrElse(Logger[F].error(show"No know hosts are set during node startup"))
      )

      notifier <- networkAlgebra.makeNotifier(peerManager, p2pNetworkConfig)
      _        <- Resource.liftK(notifier.sendNoWait(Notifier.Message.StartNotifications))

      _ <- startPeersStatusNotifier(peerManager, peersStatusChangesTopic)
      _ <- startNetworkCommands(peerManager, networkCommands)
    } yield peerManager

  private def startPeersStatusNotifier[F[_]: Async: Logger](
    peersManager: PeersManagerActor[F],
    topic:        Topic[F, PeerConnectionChange]
  ): Resource[F, F[Outcome[F, Throwable, Unit]]] =
    topic.subscribeUnbounded
      .evalTap {
        case PeerConnectionChanges.InboundConnectionInitializing(_, _) => Applicative[F].unit
        case PeerConnectionChanges.OutboundConnectionInitializing(_)   => Applicative[F].unit
        case PeerConnectionChanges.ConnectionEstablished(_, localAddress) =>
          peersManager.sendNoWait(PeersManager.Message.UpdateThisPeerAddress(localAddress))
        case PeerConnectionChanges.ConnectionClosed(connectedPeer, _) =>
          Logger[F].info(show"Remote peer ${connectedPeer.remoteAddress} closing had been detected") >>
          connectedPeer.p2pVK
            .map(b => HostId(b))
            .traverse_(id => peersManager.sendNoWait(PeersManager.Message.ClosePeer(id)))
        case PeerConnectionChanges.RemotePeerApplicationLevel(connectedPeer, appLevel) =>
          peersManager.sendNoWait(PeersManager.Message.RemotePeerNetworkLevel(HostId(connectedPeer.p2pVK), appLevel))
        case PeerConnectionChanges.ChangedRemotePeer(oldPeer, newPeer) =>
          val oldAndNewIds = for {
            oldId <- oldPeer.p2pVK
            newId <- newPeer.p2pVK
          } yield (HostId(oldId), HostId(newId))
          oldAndNewIds.traverse_ { case (oldId, newId) =>
            Logger[F].info(show"Peer $oldId changed to $newId") >>
            peersManager.sendNoWait(PeersManager.Message.RemotePeerIdChanged(oldId, newId))
          }
      }
      .compile
      .drain
      .background

  private def startNetworkCommands[F[_]: Async: Logger](
    peersManager: PeersManagerActor[F],
    topic:        Topic[F, NetworkCommands]
  ): Resource[F, F[Outcome[F, Throwable, Unit]]] =
    topic.subscribeUnbounded
      .evalTap {
        case NetworkCommands.ForgetPeer(hostId) =>
          peersManager.sendNoWait(PeersManager.Message.ForgetPeer(hostId))
        case NetworkCommands.AddPeer(remoteAddress, remotePeerIdOpt) =>
          val hostId = remotePeerIdOpt.getOrElse(HostId(ByteString.copyFrom(Random.nextBytes(hostIdBytesLen))))
          val knownPeer = KnownRemotePeer(hostId, remoteAddress, 0, 0, None)
          peersManager.sendNoWait(PeersManager.Message.AddKnownPeers(NonEmptyChain.one(knownPeer)))
      }
      .compile
      .drain
      .background

  private def buildSaveRemotePeersFunction[F[_]: Async: Logger](
    remotePeersStore: Store[F, Unit, Seq[KnownRemotePeer]]
  ): Set[KnownRemotePeer] => F[Unit] = { (peers: Set[KnownRemotePeer]) =>
    Logger[F].info(show"Going to save known hosts $peers to local data storage") >>
    remotePeersStore.put((), peers.toList)
  }

  // peers represented as Remote address could be present in remote peers as well with some reputation
  private def mergeRemotePeersAndRemoteAddress(
    remotePeers:   Seq[KnownRemotePeer],
    remoteAddress: Seq[DisconnectedPeer]
  ): Seq[KnownRemotePeer] = {
    val remoteAddressMap = remoteAddress.map { ra =>
      val id =
        ra.p2pVK.fold(HostId(ByteString.copyFrom(Random.nextBytes(hostIdBytesLen))))(HostId.apply)
      ra.remoteAddress -> KnownRemotePeer(id, ra.remoteAddress, 0, 0, None)
    }.toMap
    val remotePeersMap = remotePeers.map(p => p.address -> p).toMap

    (remoteAddressMap ++ remotePeersMap).values.toSeq
  }
}
