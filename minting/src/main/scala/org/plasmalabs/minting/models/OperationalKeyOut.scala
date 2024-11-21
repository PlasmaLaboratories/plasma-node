package org.plasmalabs.minting.models

import com.google.protobuf.ByteString
import org.plasmalabs.consensus.models.*
import org.plasmalabs.models.Slot

case class OperationalKeyOut(
  slot:            Slot,
  childVK:         ByteString,
  childSK:         ByteString,
  parentSignature: SignatureKesProduct,
  parentVK:        VerificationKeyKesProduct
)
