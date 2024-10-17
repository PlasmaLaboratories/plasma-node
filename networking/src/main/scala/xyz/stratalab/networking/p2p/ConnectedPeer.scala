package org.plasmalabs.networking.p2p

import cats.implicits._
import org.plasmalabs.models.Bytes
import org.plasmalabs.models.p2p.RemoteAddress
import org.plasmalabs.typeclasses.implicits._

case class ConnectedPeer(remoteAddress: RemoteAddress, p2pVK: Bytes, networkVersion: Bytes) {
  override def toString: String = show"ConnectedPeer(address=$remoteAddress, id=$p2pVK, version=$networkVersion)"
}

case class DisconnectedPeer(remoteAddress: RemoteAddress, p2pVK: Option[Bytes]) {
  override def toString: String = show"DisconnectedPeer(address=$remoteAddress, id=${p2pVK.fold("?")(_.show)})"
}

case class LocalPeer(localAddress: RemoteAddress, p2pVK: Bytes, p2pSK: Bytes) {
  override def toString: String = show"LocalPeer(address=$localAddress, id=$p2pVK)"
}
