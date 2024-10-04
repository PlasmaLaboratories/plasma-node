package xyz.stratalab.consensus.models

import com.google.protobuf.ByteString
import xyz.stratalab.codecs.bytes.typeclasses.Signable
import xyz.stratalab.models._

case class VrfArgument(eta: Eta, slot: Slot)

object VrfArgument {

  implicit val signableVrfArgument: Signable[VrfArgument] =
    arg => arg.eta.data.concat(ByteString.copyFrom(BigInt(arg.slot).toByteArray))
}
