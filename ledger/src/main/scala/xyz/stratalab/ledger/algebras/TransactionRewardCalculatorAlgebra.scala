package xyz.stratalab.ledger.algebras

import xyz.stratalab.ledger.models.RewardQuantities
import xyz.stratalab.sdk.models.transaction.IoTransaction

trait TransactionRewardCalculatorAlgebra {

  /**
   * Calculates the provided rewards of the given transaction.  Any "excess" value is treated as a Reward
   * @param transaction The transaction containing the fee/reward
   * @return a BigInt representing the LVL fee/reward
   */
  def rewardsOf(transaction: IoTransaction): RewardQuantities

}
