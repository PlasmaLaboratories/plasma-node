package xyz.stratalab.consensus.algebras

import xyz.stratalab.consensus.models.BlockHeaderToBodyValidationFailure
import xyz.stratalab.node.models.Block

trait BlockHeaderToBodyValidationAlgebra[F[_]] {
  def validate(block: Block): F[Either[BlockHeaderToBodyValidationFailure, Block]]
}
