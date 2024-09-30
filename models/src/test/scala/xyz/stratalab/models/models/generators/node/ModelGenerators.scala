package xyz.stratalab.models.generators.node

import co.topl.brambl.generators.ModelGenerators._
import co.topl.node.models._
import org.scalacheck.{Arbitrary, Gen}
import xyz.stratalab.models.generators.consensus.ModelGenerators.arbitraryHeader

trait ModelGenerators {

  implicit val arbitraryNodeBody: Arbitrary[BlockBody] =
    Arbitrary(
      for {
        transactions <- Gen.listOf(arbitraryTransactionId.arbitrary)
        reward       <- Gen.option(arbitraryTransactionId.arbitrary)
      } yield BlockBody(transactions, reward)
    )

  implicit val arbitraryNodeFullBody: Arbitrary[FullBlockBody] =
    Arbitrary(
      for {
        transactions <- Gen.listOf(arbitraryIoTransaction.arbitrary)
        reward       <- Gen.option(arbitraryIoTransaction.arbitrary)
      } yield FullBlockBody(transactions, reward)
    )

  implicit val arbitraryBlock: Arbitrary[Block] =
    Arbitrary(
      for {
        header <- arbitraryHeader.arbitrary
        body   <- arbitraryNodeBody.arbitrary
      } yield Block(header, body)
    )

  implicit val arbitraryFullBlock: Arbitrary[FullBlock] =
    Arbitrary(
      for {
        header <- arbitraryHeader.arbitrary
        body   <- arbitraryNodeFullBody.arbitrary
      } yield FullBlock(header, body)
    )
}
object ModelGenerators extends ModelGenerators
