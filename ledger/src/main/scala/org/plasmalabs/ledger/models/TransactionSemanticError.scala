package org.plasmalabs.ledger.models

import org.plasmalabs.models.Slot
import org.plasmalabs.sdk.models.transaction.{Schedule, SpentTransactionOutput}
import org.plasmalabs.sdk.models.{TransactionId, TransactionOutputAddress}

sealed abstract class TransactionSemanticError

object TransactionSemanticErrors {

  /**
   * The declared Transaction Input does not match the data stored on the chain
   */
  case class InputDataMismatch(input: SpentTransactionOutput) extends TransactionSemanticError

  /**
   * The given input Box ID is not currently spendable
   *   - The box may have never existed
   *   - The box may have been spent already
   */
  case class UnspendableBox(boxId: TransactionOutputAddress) extends TransactionSemanticError

  /**
   * The Transaction was included in a block with a Slot that does not satisfy the Transaction's schedule requirements
   * @param slot the slot of the block containing the transaction
   * @param schedule the transaction's schedule
   */
  case class UnsatisfiedSchedule(slot: Slot, schedule: Schedule) extends TransactionSemanticError

  /**
   * Input transaction is missed in our transaction store
   * @param id missed transaction id
   */
  case class InputTransactionIsMissed(id: TransactionId) extends TransactionSemanticError
}
