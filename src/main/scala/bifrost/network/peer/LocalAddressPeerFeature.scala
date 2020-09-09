package bifrost.network.peer

import java.net.InetSocketAddress

case class LocalAddressPeerFeature(address: InetSocketAddress) extends PeerFeature {
  override type M = LocalAddressPeerFeature
  override val featureId: PeerFeature.Id = LocalAddressPeerFeature.featureId

  override def serializer: LocalAddressPeerFeatureSerializer.type = LocalAddressPeerFeatureSerializer
}

object LocalAddressPeerFeature {
  val featureId: PeerFeature.Id = 2: Byte
}
