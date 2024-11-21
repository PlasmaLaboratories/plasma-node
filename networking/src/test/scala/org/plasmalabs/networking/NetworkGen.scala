package org.plasmalabs.networking

import com.google.protobuf.ByteString
import org.plasmalabs.models.p2p.*
import org.plasmalabs.networking.blockchain.NetworkProtocolVersions
import org.plasmalabs.networking.p2p.*
import org.scalacheck.{Arbitrary, Gen}

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
