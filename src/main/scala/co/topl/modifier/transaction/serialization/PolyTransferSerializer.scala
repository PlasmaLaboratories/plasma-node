package co.topl.modifier.transaction.serialization

import co.topl.crypto.proposition.PublicKey25519Proposition
import co.topl.crypto.proposition.serialization.PublicKey25519PropositionSerializer
import co.topl.crypto.signature.Signature25519
import co.topl.crypto.signature.serialization.Signature25519Serializer
import co.topl.modifier.transaction.PolyTransfer
import co.topl.modifier.transaction.Transaction.Nonce
import co.topl.utils.Extensions._
import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }

object PolyTransferSerializer extends BifrostSerializer[PolyTransfer] {

  override def serialize(obj: PolyTransfer, w: Writer): Unit = {
    /* from: IndexedSeq[(PublicKey25519Proposition, Nonce)] */
    w.putUInt(obj.from.length)
    obj.from.foreach { case (prop, nonce) =>
      PublicKey25519PropositionSerializer.serialize(prop, w)
      w.putLong(nonce)
    }

    /* to: IndexedSeq[(PublicKey25519Proposition, Long)] */
    w.putUInt(obj.to.length)
    obj.to.foreach { case (prop, value) =>
      PublicKey25519PropositionSerializer.serialize(prop, w)
      w.putULong(value)
    }

    /* signatures: Map[PublicKey25519Proposition, Signature25519] */
    w.putUInt(obj.signatures.size)
    obj.signatures.foreach { case (prop, sig) =>
      PublicKey25519PropositionSerializer.serialize(prop, w)
      Signature25519Serializer.serialize(sig, w)
    }

    /* fee: Long */
    w.putULong(obj.fee)

    /* timestamp: Long */
    w.putULong(obj.timestamp)

    /* data: String */
    w.putIntString(obj.data)
  }

  override def parse(r: Reader): PolyTransfer = {
    val fromLength: Int = r.getUInt().toIntExact
    val from: IndexedSeq[(PublicKey25519Proposition, Nonce)] = (0 until fromLength).map { _ =>
      val prop = PublicKey25519PropositionSerializer.parse(r)
      val nonce = r.getLong()
      prop -> nonce
    }

    val toLength: Int = r.getUInt().toIntExact
    val to: IndexedSeq[(PublicKey25519Proposition, Long)] = (0 until toLength).map { _ =>
      val prop = PublicKey25519PropositionSerializer.parse(r)
      val value = r.getULong()
      prop -> value
    }

    val signaturesLength: Int = r.getUInt().toIntExact
    val signatures: Map[PublicKey25519Proposition, Signature25519] = (0 until signaturesLength).map { _ =>
      val prop = PublicKey25519PropositionSerializer.parse(r)
      val sig = Signature25519Serializer.parse(r)
      prop -> sig
    }.toMap

    val fee: Long = r.getULong()
    val timestamp: Long = r.getULong()
    val data: String = r.getIntString()

    PolyTransfer(from, to, signatures, fee, timestamp, data)
  }
}
