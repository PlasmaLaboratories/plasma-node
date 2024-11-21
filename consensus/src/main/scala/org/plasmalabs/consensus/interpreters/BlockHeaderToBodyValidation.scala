package org.plasmalabs.consensus.interpreters

import cats.effect.kernel.Sync
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxEq}
import org.plasmalabs.consensus.algebras.BlockHeaderToBodyValidationAlgebra
import org.plasmalabs.consensus.models.BlockHeaderToBodyValidationFailure
import org.plasmalabs.consensus.models.BlockHeaderToBodyValidationFailure.IncorrectTxRoot
import org.plasmalabs.models.utility.*
import org.plasmalabs.models.utility.HasLength.instances.*
import org.plasmalabs.node.models.Block
import org.plasmalabs.typeclasses.implicits.*

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
