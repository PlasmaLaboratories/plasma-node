package xyz.stratalab.transactiongenerator.algebras

import xyz.stratalab.transactiongenerator.models.Wallet

trait WalletInitializer[F[_]] {

  /**
   * Create a Wallet that contains spendable UTxOs
   */
  def initialize: F[Wallet]
}
