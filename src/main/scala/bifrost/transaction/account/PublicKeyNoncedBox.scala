package bifrost.transaction.account

import com.google.common.primitives.Longs
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.transaction.box.Box
import bifrost.transaction.box.proposition.PublicKey25519Proposition

trait PublicKeyNoncedBox[PKP <: PublicKey25519Proposition] extends Box[PKP] {
  val nonce: Long

  lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val publicKey = proposition

  override def equals(obj: Any): Boolean = obj match {
    case acc: PublicKeyNoncedBox[PKP] => (acc.id sameElements this.id) && acc.value == this.value
    case _ => false
  }

  override def hashCode(): Int = proposition.hashCode()
}

object PublicKeyNoncedBox {

  def idFromBox[PKP <: PublicKey25519Proposition](prop: PKP, nonce: Long): Array[Byte] =
    FastCryptographicHash(prop.pubKeyBytes ++ Longs.toByteArray(nonce))
}