package org.plasmalabs.consensus.algebras

import org.plasmalabs.consensus.models.{BlockHeader, BlockHeaderValidationFailure}

trait BlockHeaderVotingValidationAlgebra[F[_]] {

  /**
   * Indicates if Block header contains correct voting information
   */
  def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]]
}
