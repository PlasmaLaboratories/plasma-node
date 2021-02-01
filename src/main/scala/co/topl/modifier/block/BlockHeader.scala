package co.topl.modifier.block

import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.crypto.Digest32Ops
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.Block.Timestamp
import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.nodeView.state.box.ArbitBox
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import scorex.crypto.hash.Digest32
import supertagged.@@

case class BlockHeader(id          : ModifierId,
                       parentId    : ModifierId,
                       timestamp   : Timestamp,
                       generatorBox: ArbitBox,
                       publicKey   : PublicKeyPropositionCurve25519,
                       signature   : SignatureCurve25519,
                       height      : Long,
                       difficulty  : Long,
                       txRoot      : Digest32,
                       bloomFilter : BloomFilter,
                       version     : PNVMVersion
                      ) extends PersistentNodeViewModifier {

  override lazy val modifierTypeId: ModifierTypeId = BlockHeader.modifierTypeId

}

object BlockHeader {
  val modifierTypeId: Byte @@ NodeViewModifier.ModifierTypeId.Tag = ModifierTypeId @@ (4: Byte)


  implicit val jsonEncoder: Encoder[BlockHeader] = { bh: BlockHeader ⇒
    Map(
      "id" -> bh.id.toString.asJson,
      "parentId" -> bh.parentId.toString.asJson,
      "timestamp" -> bh.timestamp.asJson,
      "generatorBox" -> bh.generatorBox.asJson,
      "publicKey" -> bh.publicKey.asJson,
      "signature" -> bh.signature.asJson,
      "height" -> bh.height.asJson,
      "difficulty" -> bh.difficulty.asJson,
      "txRoot" -> bh.txRoot.asJson(Digest32Ops.jsonEncoder),
      "bloomFilter" -> bh.bloomFilter.asJson,
      "version" -> bh.version.asJson,
    ).asJson
  }

  implicit val jsonDecoder: Decoder[BlockHeader] = (c: HCursor) =>
    for {
      id <- c.downField("id").as[ModifierId]
      parentId <- c.downField("parentId").as[ModifierId]
      timestamp <- c.downField("timestamp").as[Timestamp]
      generatorBox <- c.downField("generatorBox").as[ArbitBox]
      publicKey <- c.downField("publicKey").as[PublicKeyPropositionCurve25519]
      signature <- c.downField("signature").as[SignatureCurve25519]
      height <- c.downField("height").as[Long]
      difficulty <- c.downField("difficulty").as[Long]
      txRoot <- c.downField("txRoot").as[Digest32](Digest32Ops.jsonDecoder)
      bloomFilter <- c.downField("bloomFilter").as[BloomFilter]
      version <- c.downField("version").as[Byte]
    } yield {
      BlockHeader(
        id,
        parentId,
        timestamp,
        generatorBox,
        publicKey,
        signature,
        height,
        difficulty,
        txRoot,
        bloomFilter,
        version
      )
    }
}
