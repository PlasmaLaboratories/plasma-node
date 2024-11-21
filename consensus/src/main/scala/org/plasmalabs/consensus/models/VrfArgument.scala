package org.plasmalabs.consensus.models

import com.google.protobuf.ByteString
import org.plasmalabs.codecs.bytes.typeclasses.Signable
import org.plasmalabs.models.*

case class VrfArgument(eta: Eta, slot: Slot)

object VrfArgument {

  implicit val signableVrfArgument: Signable[VrfArgument] =
    arg => arg.eta.data.concat(ByteString.copyFrom(BigInt(arg.slot).toByteArray))
}
