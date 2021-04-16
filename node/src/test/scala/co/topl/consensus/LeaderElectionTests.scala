package co.topl.consensus

import co.topl.attestation.Address
import co.topl.consensus.LeaderElection.{NoAddressesAvailable, NoArbitBoxesAvailable}
import co.topl.utils.{CoreGenerators, NetworkType}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

class LeaderElectionTests extends AnyFlatSpec with MockFactory with CoreGenerators {

  val address: Address =
    Address(NetworkType.PrivateTestnet.netPrefix)("AUAvJqLKc8Un3C6bC4aj8WgHZo74vamvX8Kdm6MhtdXgw51cGfix")

  "getEligibleBox" should "return NoAddressesAvailable when no addresses provided" in {
    forAll(blockGen) { parent =>
      val stateReader = mock[LeaderElection.SR]
      val addresses = Set[Address]()
      val expectedResult = Left(NoAddressesAvailable)

      val result = LeaderElection.getEligibleBox(parent, addresses, parent.timestamp + 100, stateReader)

      result shouldBe expectedResult
    }
  }

  "getEligibleBox" should "return NoArbitBoxesAvailable when no addresses contain arbit boxes" in {
    forAll(blockGen) { parent =>
      val stateReader = mock[LeaderElection.SR]
      (stateReader.getTokenBoxes _)
        .expects(address)
        .returns(None)
      val addresses = Set[Address](address)
      val expectedResult = Left(NoArbitBoxesAvailable)

      val result = LeaderElection.getEligibleBox(parent, addresses, parent.timestamp + 100, stateReader)

      result shouldBe expectedResult
    }
  }

}
