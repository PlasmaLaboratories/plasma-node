package xyz.stratalab.networking

import com.google.protobuf.ByteString
import org.scalacheck.{Arbitrary, Gen}
import xyz.stratalab.models.p2p._
import xyz.stratalab.networking.blockchain.NetworkProtocolVersions
import xyz.stratalab.networking.p2p._

trait NetworkGen {

  implicit val arbitraryRemoteAddress: Arbitrary[RemoteAddress] =
    Arbitrary(Gen.chooseNum[Int](0, 65535).map(port => RemoteAddress("localhost", port)))

  implicit val arbitraryConnectedPeer: Arbitrary[ConnectedPeer] =
    Arbitrary(
      for {
        address <- arbitraryRemoteAddress.arbitrary
        peerVK  <- Gen.containerOfN[Array, Byte](32, Arbitrary.arbByte.arbitrary)
      } yield ConnectedPeer(address, ByteString.copyFrom(peerVK), NetworkProtocolVersions.V1)
    )

}

object NetworkGen extends NetworkGen
