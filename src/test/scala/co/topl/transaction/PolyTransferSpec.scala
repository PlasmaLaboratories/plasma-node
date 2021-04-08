package co.topl.transaction

import co.topl.modifier.transaction.PolyTransfer
import co.topl.utils.{CoreGenerators, ValidGenerators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class PolyTransferSpec
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with CoreGenerators
    with ValidGenerators {

  property("Generated PolyTransfer Tx should be valid") {
    forAll(validPolyTransferGen) { polyTransfer =>
      //TODO: Jing - change this back to using syntacticValidate once attestation in validPolyTransferGen works
      polyTransfer.rawValidate shouldBe 'valid
    }
  }

  property("Attempting to validate a PolyTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(polyTransferGen) { polyTransfer =>
      polyTransfer.syntacticValidate shouldBe 'invalid
    }
  }
}
