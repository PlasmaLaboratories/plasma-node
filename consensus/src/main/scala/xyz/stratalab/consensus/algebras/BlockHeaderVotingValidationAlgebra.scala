package xyz.stratalab.consensus.algebras

import xyz.stratalab.consensus.models.{BlockHeader, BlockHeaderValidationFailure}

trait BlockHeaderVotingValidationAlgebra[F[_]] {

  /**
   * Indicates if Block header contains correct voting information
   */
  def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]]
}
