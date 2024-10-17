package org.plasmalabs.consensus.algebras

import org.plasmalabs.consensus.models.{BlockHeader, BlockHeaderValidationFailure}

trait BlockHeaderVersionValidationAlgebra[F[_]] {

  /**
   * Indicates if Block header have correct version
   */
  def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]]
}
