package bifrost.transaction.bifrostTransaction

import java.time.Instant

import bifrost.crypto.hash.FastCryptographicHash
import bifrost.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.box.{AssetBox, BifrostBox, BoxUnlocker}
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.serialization.AssetCreationCompanion
import bifrost.wallet.BWallet
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

case class AssetCreation (to: IndexedSeq[(PublicKey25519Proposition, Long)],
                          signatures: IndexedSeq[Signature25519],
                          assetCode: String,
                          val issuer: PublicKey25519Proposition,
                          override val fee: Long,
                          override val timestamp: Long,
                          val data: String) extends BifrostTransaction {


  override type M = AssetCreation

  lazy val serializer = AssetCreationCompanion

  override def toString: String = s"AssetCreation(${json.noSpaces})"

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq()

  override lazy val unlockers: Traversable[BoxUnlocker[ProofOfKnowledgeProposition[PrivateKey25519]]] = Traversable()

  lazy val hashNoNonces = FastCryptographicHash(
  to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
    Longs.toByteArray(timestamp) ++
    Longs.toByteArray(fee)
  )

  override lazy val newBoxes: Traversable[BifrostBox] = to.zipWithIndex.map {
   case ((prop, value), idx) =>
     val nonce = AssetCreation.nonceFromDigest(FastCryptographicHash(
       "AssetCreation".getBytes ++
         prop.pubKeyBytes ++
         issuer.pubKeyBytes ++
         assetCode.getBytes ++
         hashNoNonces ++
         Ints.toByteArray(idx)
     ))

     //TODO assetBoxes elsewhere do not subtract fee from box value
     //TODO no check that amount >= fee
     //AssetBox(prop, nonce, value, assetCode, hub)
     AssetBox(prop, nonce, value - fee, assetCode, issuer, data)
   }

  override lazy val json: Json = Map(
    "txHash" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
    "to" -> to.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
        "value" -> s._2.asJson
      ).asJson
    }.asJson,
    "issuer" -> Base58.encode(issuer.pubKeyBytes).asJson,
    "assetCode" -> assetCode.asJson,
    "signatures" -> signatures.map(s => Base58.encode(s.signature).asJson).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson,
    "data" -> data.asJson
  ).asJson

  def commonMessageToSign: Array[Byte] = (if (newBoxes.nonEmpty) {
  newBoxes
    .map(_.bytes)
    .reduce(_ ++ _)
  } else {
    Array[Byte]()
  }) ++
    Longs.toByteArray(timestamp) ++
    Longs.toByteArray(fee)

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
  "AssetCreation".getBytes(),
  commonMessageToSign,
  issuer.pubKeyBytes,
  assetCode.getBytes,
  data.getBytes
  )

}

object AssetCreation {

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  def validate(tx: AssetCreation): Try[Unit] = Try {
    //require(tx.from.size == tx.signatures.size)
    require(tx.to.forall(_._2 >= 0L))
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.signatures.forall({ case (signature) =>
      //println(signature.isValid(tx.hub, tx.messageToSign))
      signature.isValid(tx.issuer, tx.messageToSign)
    }), "Invalid signatures")
  }

  /**
    * Route here from AssetApiRoute
    * Assumes that the Wallet contains the issuer's key information
    * Takes Wallet from current view, and generates signature from issuer's public key
    * Forms corresponding AssetCreation transaction
    */
  def createAndApply(w: BWallet,
                     to: IndexedSeq[(PublicKey25519Proposition, Long)],
                     fee: Long,
                     issuer: PublicKey25519Proposition,
                     assetCode: String,
                     data: String): Try[AssetCreation] = Try {

    val selectedSecret = w.secretByPublicImage(issuer).get
    val fakeSigs = IndexedSeq(Signature25519(Array()))
    val timestamp = Instant.now.toEpochMilli
    val messageToSign = AssetCreation(to, fakeSigs, assetCode, issuer, fee, timestamp, data).messageToSign

    val signatures = IndexedSeq(PrivateKey25519Companion.sign(selectedSecret, messageToSign))

    AssetCreation(to, signatures, assetCode, issuer, fee, timestamp, data)
  }
}