package xyz.stratalab.consensus.algebras

import co.topl.consensus.models.BlockHeader
import xyz.stratalab.consensus.models.BlockHeaderValidationFailure

trait BlockHeaderVersionValidationAlgebra[F[_]] {

  /**
   * Indicates if Block header have correct version
   */
  def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]]
}
