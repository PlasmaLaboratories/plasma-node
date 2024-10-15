package xyz.stratalab.transactiongenerator.models

import xyz.stratalab.sdk.models.box.{Box, Lock}
import xyz.stratalab.sdk.models.{LockAddress, TransactionOutputAddress}

case class Wallet(
  spendableBoxes: Map[TransactionOutputAddress, Box],
  propositions:   Map[LockAddress, Lock]
)
