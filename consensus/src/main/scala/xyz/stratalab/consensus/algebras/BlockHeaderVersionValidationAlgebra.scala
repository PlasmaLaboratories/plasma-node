package xyz.stratalab.consensus.algebras

import xyz.stratalab.consensus.models.{BlockHeader, BlockHeaderValidationFailure}

trait BlockHeaderVersionValidationAlgebra[F[_]] {

  /**
   * Indicates if Block header have correct version
   */
  def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]]
}
