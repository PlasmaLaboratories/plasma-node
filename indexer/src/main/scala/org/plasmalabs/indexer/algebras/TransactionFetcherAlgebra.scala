package org.plasmalabs.indexer.algebras

import org.plasmalabs.indexer.model.GE
import org.plasmalabs.indexer.services.{TransactionReceipt, Txo, TxoState}
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.models.{LockAddress, TransactionId}

/**
 * Algebra which defines fetch operations of transactions against the stored Ledger.
 *
 * @tparam F the effect-ful context to retrieve the value in
 */
trait TransactionFetcherAlgebra[F[_]] {

  /**
   * Fetch a Transaction on the stored Ledger
   * @param transactionId  Transaction Identifier filter by field
   * @return Optional Transaction, None if it was not found
   */
  def fetchTransaction(transactionId: TransactionId): F[Either[GE, Option[IoTransaction]]]

  /**
   * Fetch a Transaction TransactionReceipt (includes ioTx, blockId, chain distance,...) on the stored Ledger
   *
   * @param transactionId Transaction Identifier filter by field
   * @return Optional Transaction, None if it was not found
   */
  def fetchTransactionReceipt(transactionId: TransactionId): F[Either[GE, Option[TransactionReceipt]]]

  /**
   * Retrieve TxOs (spent or unspent) that are associated with any of the specified addresses
   *
   * @param lockAddress the lock address
   * @param state TxoState filterb y field
   * @return Txos, an empty sequence if lockAddress and state filter matches empty results
   */
  def fetchTransactionByLockAddress(lockAddress: LockAddress, state: TxoState): F[Either[GE, List[Txo]]]

}
