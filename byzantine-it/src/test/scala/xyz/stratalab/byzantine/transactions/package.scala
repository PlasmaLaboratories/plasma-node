package xyz.stratalab.byzantine

import xyz.stratalab.sdk.models.{LockAddress, TransactionOutputAddress}
import xyz.stratalab.sdk.models.box.{Box, Lock}

/**
 * Package transactions try to group methods and variables used on byzantine-it.TransactionTest
 */
package object transactions {

  case class Wallet(spendableBoxes: Map[TransactionOutputAddress, Box], propositions: Map[LockAddress, Lock])
}
