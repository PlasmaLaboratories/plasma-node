package org.plasmalabs.consensus.models

import com.google.protobuf.ByteString
import org.plasmalabs.models.{Bytes, Epoch, Eta, RhoNonceHash}

case class EtaCalculationArgs(previousEta: Eta, epoch: Epoch, rhoNonceHashValues: Iterable[RhoNonceHash]) {

  def digestMessages: List[Bytes] =
    previousEta.data +:
    ByteString.copyFrom(BigInt(epoch).toByteArray) +:
    rhoNonceHashValues.map(_.sizedBytes.data).toList
}
