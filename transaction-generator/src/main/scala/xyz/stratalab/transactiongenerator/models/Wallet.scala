package org.plasmalabs.transactiongenerator.models

import org.plasmalabs.sdk.models.box.{Box, Lock}
import org.plasmalabs.sdk.models.{LockAddress, TransactionOutputAddress}

case class Wallet(
  spendableBoxes: Map[TransactionOutputAddress, Box],
  propositions:   Map[LockAddress, Lock]
)
