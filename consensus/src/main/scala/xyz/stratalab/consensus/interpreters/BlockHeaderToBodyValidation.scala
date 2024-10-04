package xyz.stratalab.consensus.interpreters

import cats.effect.kernel.Sync
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxEq}
import xyz.stratalab.consensus.algebras.BlockHeaderToBodyValidationAlgebra
import xyz.stratalab.consensus.models.BlockHeaderToBodyValidationFailure
import xyz.stratalab.consensus.models.BlockHeaderToBodyValidationFailure.IncorrectTxRoot
import xyz.stratalab.models.utility.HasLength.instances._
import xyz.stratalab.models.utility._
import xyz.stratalab.node.models.Block
import xyz.stratalab.typeclasses.implicits._

object BlockHeaderToBodyValidation {

  def make[F[_]: Sync](): F[BlockHeaderToBodyValidationAlgebra[F]] = Sync[F].delay(new Impl[F]())

  private class Impl[F[_]: Sync]() extends BlockHeaderToBodyValidationAlgebra[F] {

    override def validate(block: Block): F[Either[BlockHeaderToBodyValidationFailure, Block]] =
      blockTxRootConsistent(block).pure[F]
  }

  private def blockTxRootConsistent(block: Block): Either[BlockHeaderToBodyValidationFailure, Block] = {
    val bodyMerkleTxRoot = block.body.merkleTreeRootHash
    val headerMerkleTxRoot = block.header.txRoot

    if (bodyMerkleTxRoot === Sized.strictUnsafe(headerMerkleTxRoot)) {
      Right(block)
    } else {
      Left(IncorrectTxRoot(Sized.strictUnsafe(headerMerkleTxRoot), bodyMerkleTxRoot))
    }
  }

}
