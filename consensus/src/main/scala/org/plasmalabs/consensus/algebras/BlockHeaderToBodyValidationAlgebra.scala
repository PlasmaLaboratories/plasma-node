package org.plasmalabs.consensus.algebras

import org.plasmalabs.consensus.models.BlockHeaderToBodyValidationFailure
import org.plasmalabs.node.models.Block

trait BlockHeaderToBodyValidationAlgebra[F[_]] {
  def validate(block: Block): F[Either[BlockHeaderToBodyValidationFailure, Block]]
}
