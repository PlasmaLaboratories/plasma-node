package xyz.stratalab.minting.models

import co.topl.consensus.models._
import com.google.protobuf.ByteString
import xyz.stratalab.models.Slot

case class OperationalKeyOut(
  slot:            Slot,
  childVK:         ByteString,
  childSK:         ByteString,
  parentSignature: SignatureKesProduct,
  parentVK:        VerificationKeyKesProduct
)
