package org.plasmalabs.minting.algebras

import fs2.Stream
import org.plasmalabs.node.models.FullBlock

/**
 * Perpetually mints new blocks
 * @tparam F[_] Base type constructor
 */
trait BlockProducerAlgebra[F[_]] {
  def blocks: F[Stream[F, FullBlock]]
}
