package org.plasmalabs.networking.fsnetwork

sealed trait PeerState {
  def networkLevel: Boolean
  def applicationLevel: Boolean

  def isActive: Boolean = networkLevel || applicationLevel
}

object PeerState {

  // Peer was not known for us, i.e. no record for peer at all
  case object Unknown extends PeerState {
    override def networkLevel: Boolean = false
    override def applicationLevel: Boolean = false
  }

  case object Banned extends PeerState {
    override def networkLevel: Boolean = false
    override def applicationLevel: Boolean = false
  }

  case object Cold extends PeerState {
    override def networkLevel: Boolean = false
    override def applicationLevel: Boolean = false
  }

  case object Warm extends PeerState {
    override def networkLevel: Boolean = true
    override def applicationLevel: Boolean = false
  }

  case object Hot extends PeerState {
    override def networkLevel: Boolean = true
    override def applicationLevel: Boolean = true
  }

}
