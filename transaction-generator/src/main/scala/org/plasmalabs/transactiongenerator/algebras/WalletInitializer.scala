package org.plasmalabs.transactiongenerator.algebras

import org.plasmalabs.transactiongenerator.models.Wallet

trait WalletInitializer[F[_]] {

  /**
   * Create a Wallet that contains spendable UTxOs
   */
  def initialize: F[Wallet]
}
