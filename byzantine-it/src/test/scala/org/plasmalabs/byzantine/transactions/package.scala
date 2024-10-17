package org.plasmalabs.byzantine

import org.plasmalabs.sdk.models.{LockAddress, TransactionOutputAddress}
import org.plasmalabs.sdk.models.box.{Box, Lock}

/**
 * Package transactions try to group methods and variables used on byzantine-it.TransactionTest
 */
package object transactions {

  case class Wallet(spendableBoxes: Map[TransactionOutputAddress, Box], propositions: Map[LockAddress, Lock])
}
