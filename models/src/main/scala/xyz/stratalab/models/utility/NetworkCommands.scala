package xyz.stratalab.models.utility

import xyz.stratalab.models.p2p.{HostId, RemoteAddress}

sealed trait NetworkCommands

object NetworkCommands {
  case class ForgetPeer(hostId: HostId) extends NetworkCommands
  case class AddPeer(remoteAddress: RemoteAddress, remotePeerIdOpt: Option[HostId]) extends NetworkCommands
}
