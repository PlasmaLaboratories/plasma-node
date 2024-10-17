package org.plasmalabs.ledger.algebras

import org.plasmalabs.consensus.models.BlockId
import org.plasmalabs.sdk.models.TransactionOutputAddress

trait BoxStateAlgebra[F[_]] {

  /**
   * Indicates if a particular box is spendable at the given block ID
   * @return F[true] if the box exists and is spendable
   *         F[false] if the box either never existed or has been spent already.  The interpretation should make no
   *         distinction between the two scenarios.
   */
  def boxExistsAt(blockId: BlockId)(boxId: TransactionOutputAddress): F[Boolean]
}
