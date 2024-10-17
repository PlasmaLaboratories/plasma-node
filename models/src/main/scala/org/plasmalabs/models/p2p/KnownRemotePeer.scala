package org.plasmalabs.models.p2p

case class KnownRemotePeer(
  peerId:              HostId,
  address:             RemoteAddress,
  blockReputation:     HostReputationValue,
  perfReputation:      HostReputationValue,
  lastOpenedTimestamp: Option[Long]
)
