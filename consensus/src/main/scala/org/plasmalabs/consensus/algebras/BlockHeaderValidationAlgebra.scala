package org.plasmalabs.consensus.algebras

import org.plasmalabs.consensus.models.{BlockHeader, BlockHeaderValidationFailure, SlotData}

trait BlockHeaderValidationAlgebra[F[_]] {

  /**
   * Indicates if the claimed child is a valid descendent of the parent
   */
  def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]]

  def couldBeValidated(header: BlockHeader, currentHead: SlotData): F[Boolean]
}
