package xyz.stratalab.transactiongenerator.models

import co.topl.brambl.models.box.{Box, Lock}
import co.topl.brambl.models.{LockAddress, TransactionOutputAddress}

case class Wallet(
  spendableBoxes: Map[TransactionOutputAddress, Box],
  propositions:   Map[LockAddress, Lock]
)
