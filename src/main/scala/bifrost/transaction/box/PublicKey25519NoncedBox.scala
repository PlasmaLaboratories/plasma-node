package bifrost.transaction.box

import com.google.common.primitives.Longs
import io.circe.Json
import io.circe.syntax._
import bifrost.serialization.{JsonSerializable, Serializer}
import bifrost.transaction.account.PublicKeyNoncedBox
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.util.Try

case class PublicKey25519NoncedBox(override val proposition: PublicKey25519Proposition,
                                   override val nonce: Long,
                                   override val value: Long
                                  ) extends PublicKeyNoncedBox[PublicKey25519Proposition] with JsonSerializable {

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "address" -> proposition.address.asJson,
    "publicKey" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "nonce" -> nonce.toString.asJson,
    "value" -> value.toString.asJson
  ).asJson

  override type M = PublicKey25519NoncedBox

  override def serializer: Serializer[PublicKey25519NoncedBox] = PublicKey25519NoncedBoxSerializer
}

object PublicKey25519NoncedBoxSerializer extends Serializer[PublicKey25519NoncedBox] {

  val PublicKey25519NoncedBoxLength = Curve25519.KeyLength + 16

  override def toBytes(obj: PublicKey25519NoncedBox): Array[Byte] = {
    obj.proposition.pubKeyBytes ++ Longs.toByteArray(obj.nonce) ++ Longs.toByteArray(obj.value)
  }

  override def parseBytes(bytes: Array[Byte]): Try[PublicKey25519NoncedBox] = Try {
    val pk = PublicKey25519Proposition(bytes.take(32))
    val nonce = Longs.fromByteArray(bytes.slice(32, 40))
    val value = Longs.fromByteArray(bytes.slice(40, 48))
    PublicKey25519NoncedBox(pk, nonce, value)
  }
}

