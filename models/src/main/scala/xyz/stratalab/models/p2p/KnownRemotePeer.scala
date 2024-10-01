package xyz.stratalab.models.p2p

case class KnownRemotePeer(
  peerId:              HostId,
  address:             RemoteAddress,
  blockReputation:     HostReputationValue,
  perfReputation:      HostReputationValue,
  lastOpenedTimestamp: Option[Long]
)
