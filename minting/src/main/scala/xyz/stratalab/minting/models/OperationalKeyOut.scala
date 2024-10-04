package xyz.stratalab.minting.models

import com.google.protobuf.ByteString
import xyz.stratalab.consensus.models._
import xyz.stratalab.models.Slot

case class OperationalKeyOut(
  slot:            Slot,
  childVK:         ByteString,
  childSK:         ByteString,
  parentSignature: SignatureKesProduct,
  parentVK:        VerificationKeyKesProduct
)
