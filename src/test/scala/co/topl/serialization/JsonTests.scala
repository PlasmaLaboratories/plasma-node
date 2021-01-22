package co.topl.serialization

import co.topl.attestation.{Address, Evidence, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.crypto.KeyfileCurve25519
import co.topl.modifier.ModifierId
import co.topl.modifier.block.{Block, BlockBody, BlockHeader, BloomFilter}
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.box._
import co.topl.utils.{CoreGenerators, ValidGenerators}
import com.google.common.primitives.Longs
import io.circe.syntax.EncoderOps
import org.scalacheck.Gen
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.util.encode.Base58

class JsonTests extends AnyPropSpec
  with Matchers
  with ScalaCheckPropertyChecks
  with CoreGenerators
  with ValidGenerators {

  property("PublicKey25519Proposition json") {
    forAll(propositionGen) { prop =>
      prop.asJson.as[PublicKeyPropositionCurve25519] shouldEqual Right(prop)
    }
  }

  property("SignatureCurve25519 json") {
    forAll(signatureGen) { sig =>
      sig.asJson.as[SignatureCurve25519] shouldEqual Right(sig)
    }
  }

  property("KeyfileCurve25519 json") {
    forAll(key25519Gen) { key =>
      val keyfile = KeyfileCurve25519.encryptSecret(key._1, "test")
      keyfile.asJson.as[KeyfileCurve25519] match {
        case Right(kf) =>
          kf.address shouldEqual keyfile.address
          kf.cipherText sameElements keyfile.cipherText
          kf.mac sameElements keyfile.mac
          kf.salt sameElements keyfile.salt
          kf.iv sameElements keyfile.iv
        case Left(e) => e
      }
    }
  }

  property("Address json") {
    forAll(addressGen) { address =>
      address.asJson.as[Address] shouldEqual Right(address)
    }
  }

  property("Evidence json") {
    forAll(evidenceGen) { evidence =>
      evidence.asJson.as[Evidence] shouldEqual Right(evidence)
    }
  }

  property("ModifierId json") {
    forAll(modifierIdGen) { id =>
      id.asJson.as[ModifierId] shouldEqual Right(id)
    }
  }

  property("AssetCode json") {
    forAll(assetCodeGen) { code =>
      code.asJson.as[AssetCode] shouldEqual Right(code)
    }
  }

  property("SecurityRoot json") {
    forAll(securityRootGen) { root =>
      root.asJson.as[SecurityRoot] shouldEqual Right(root)
    }
  }

  property("TokenValueHolder json") {
    forAll(Gen.oneOf(simpleValueGen, assetValueGen)) { value: TokenValueHolder =>
      value.asJson.as[TokenValueHolder] shouldEqual Right(value)
    }
  }

  property("PolyBox json") {
    forAll(polyBoxGen) { box =>
      box.asJson.as[PolyBox] shouldEqual Right(box)
    }
  }

  property("ArbitBox json") {
    forAll(arbitBoxGen) { box =>
      box.asJson.as[ArbitBox] shouldEqual Right(box)
    }
  }

  property("AssetBox json") {
    forAll(assetBoxGen) { box =>
      box.asJson.as[AssetBox] shouldEqual Right(box)
    }
  }

  property("ProgramId json") {
    forAll(programIdGen) { id =>
      id.asJson.as[ProgramId] shouldEqual Right(id)
    }
  }

  property("StateBox json") {
    forAll(stateBoxGen) { box =>
      box.asJson.as[StateBox] shouldEqual Right(box)
    }
  }

  property("CodeBox json") {
    forAll(codeBoxGen) { box =>
      box.asJson.as[CodeBox] shouldEqual Right(box)
    }
  }

  property("ExecutionBox json") {
    forAll(executionBoxGen) { box =>
      box.asJson.as[ExecutionBox] shouldEqual Right(box)
    }
  }

  property("Transaction json") {
    forAll(Gen.oneOf(polyTransferGen, arbitTransferGen, assetTransferGen)) {
      tx: Transaction.TX =>
        tx.asJson.as[Transaction.TX] shouldEqual Right(tx)
    }
  }

  property("FullBlock json") {
    forAll(blockGen) { block =>
      block.asJson.as[Block] shouldEqual Right(block)
    }
  }

  property("BlockHeader json") {
    forAll(blockGen) { block =>
      block.toComponents._1.asJson.as[BlockHeader] shouldEqual Right(block.toComponents._1)
    }
  }

  property("BlockBody json") {
    forAll(blockGen) { block =>
      block.toComponents._2.asJson.as[BlockBody] shouldEqual Right(block.toComponents._2)
    }
  }

  property("BloomFilter json") {
    forAll(blockGen) { block =>
      block.bloomFilter.asJson.as[BloomFilter] shouldEqual Right(block.bloomFilter)
    }
  }
}
