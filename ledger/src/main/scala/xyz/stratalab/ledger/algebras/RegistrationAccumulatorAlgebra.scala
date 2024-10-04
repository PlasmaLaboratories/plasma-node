package xyz.stratalab.ledger.algebras

import xyz.stratalab.consensus.models.{BlockId, StakingAddress}

trait RegistrationAccumulatorAlgebra[F[_]] {

  /**
   * Determines if the given address is contained in the set of active addresses
   * @param address the address in question
   * @return true if the address exists in the set, false otherwise
   */
  def contains(blockId: BlockId)(address: StakingAddress): F[Boolean]

}
