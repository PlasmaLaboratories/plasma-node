package xyz.stratalab.networking.fsnetwork

import xyz.stratalab.networking.p2p.DisconnectedPeer

abstract class PeerCreationRequestAlgebra[F[_]] {
  def requestNewPeerCreation(disconnectedPeer: DisconnectedPeer): F[Unit]
}

object PeerCreationRequestAlgebra {

  def apply[F[_]](peerCreationFun: DisconnectedPeer => F[Unit]): PeerCreationRequestAlgebra[F] =
    (disconnectedPeer: DisconnectedPeer) => peerCreationFun(disconnectedPeer)
}
