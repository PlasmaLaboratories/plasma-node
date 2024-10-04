package xyz.stratalab.consensus.algebras

import xyz.stratalab.consensus.models.{BlockHeader, BlockHeaderValidationFailure, SlotData}

trait BlockHeaderValidationAlgebra[F[_]] {

  /**
   * Indicates if the claimed child is a valid descendent of the parent
   */
  def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]]

  def couldBeValidated(header: BlockHeader, currentHead: SlotData): F[Boolean]
}
