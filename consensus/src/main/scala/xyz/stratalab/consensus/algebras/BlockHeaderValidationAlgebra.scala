package xyz.stratalab.consensus.algebras

import co.topl.consensus.models.{BlockHeader, SlotData}
import xyz.stratalab.consensus.models.BlockHeaderValidationFailure

trait BlockHeaderValidationAlgebra[F[_]] {

  /**
   * Indicates if the claimed child is a valid descendent of the parent
   */
  def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]]

  def couldBeValidated(header: BlockHeader, currentHead: SlotData): F[Boolean]
}
