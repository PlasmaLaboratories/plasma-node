package xyz.stratalab.consensus.algebras

import co.topl.node.models.Block
import xyz.stratalab.consensus.models.BlockHeaderToBodyValidationFailure

trait BlockHeaderToBodyValidationAlgebra[F[_]] {
  def validate(block: Block): F[Either[BlockHeaderToBodyValidationFailure, Block]]
}
