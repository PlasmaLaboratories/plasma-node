package co.topl.minting.algebras

import co.topl.catsakka.Iterative
import co.topl.models.{BlockBody, TypedIdentifier}

/**
 * Assembles an ideal Block Body using the given parent Block ID.
 */
trait BlockPackerAlgebra[F[_]] {

  /**
   * Constructs an `Iterative` which improves a given Block Body
   */
  def improvePackedBlock(parentBlockId: TypedIdentifier, height: Long, slot: Long): F[Iterative[F, BlockBody.Full]]
}