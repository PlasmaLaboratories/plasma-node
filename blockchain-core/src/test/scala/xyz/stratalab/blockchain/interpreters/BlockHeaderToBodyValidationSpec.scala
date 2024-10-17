package org.plasmalabs.blockchain.interpreters

import cats.effect.IO
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.plasmalabs.consensus.interpreters.BlockHeaderToBodyValidation
import org.plasmalabs.consensus.models.BlockHeaderToBodyValidationFailure.IncorrectTxRoot
import org.plasmalabs.models.generators.node.ModelGenerators._
import org.plasmalabs.node.models._
import org.plasmalabs.typeclasses.implicits._

class BlockHeaderToBodyValidationSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("validation should fail if block header txRoot is not match block body, i.e. block is arbitrary") {
    PropF.forAllF { block: Block =>
      withMock {
        for {
          underTest <- BlockHeaderToBodyValidation.make[F]()
          result    <- underTest.validate(block)
          _         <- IO(result.left.exists(_.isInstanceOf[IncorrectTxRoot])).assert
        } yield ()
      }
    }
  }

  test("validation should success if block header txRoot is match header body") {
    PropF.forAllF { block: Block =>
      val merkleRootHash = block.body.merkleTreeRootHash
      val correctBlock = block.copy(header = block.header.copy(txRoot = merkleRootHash.data))
      withMock {
        for {
          underTest <- BlockHeaderToBodyValidation.make[F]()
          result    <- underTest.validate(correctBlock)
          _         <- IO(result.exists(_ == correctBlock)).assert
        } yield ()
      }
    }
  }
}
