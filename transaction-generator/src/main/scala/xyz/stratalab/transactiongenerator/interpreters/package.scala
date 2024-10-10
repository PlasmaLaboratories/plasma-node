package xyz.stratalab.transactiongenerator

import quivr.models._
import xyz.stratalab.sdk.constants.NetworkConstants
import xyz.stratalab.sdk.models._
import xyz.stratalab.sdk.models.box._
import xyz.stratalab.sdk.models.transaction.IoTransaction
import xyz.stratalab.sdk.syntax._
import xyz.stratalab.transactiongenerator.models.Wallet

package object interpreters {

  val HeightLockOneProposition: Proposition =
    Proposition(
      Proposition.Value.HeightRange(
        Proposition.HeightRange("header", 1, Long.MaxValue)
      )
    )

  val HeightLockOneChallenge: Challenge =
    Challenge().withRevealed(HeightLockOneProposition)

  val HeightLockOneLock: Lock =
    Lock(
      Lock.Value.Predicate(
        Lock.Predicate(
          List(HeightLockOneChallenge),
          1
        )
      )
    )

  val HeightLockOneSpendingAddress: LockAddress =
    HeightLockOneLock.lockAddress(
      NetworkConstants.PRIVATE_NETWORK_ID,
      NetworkConstants.MAIN_LEDGER_ID
    )

  val emptyWallet: Wallet =
    Wallet(
      Map.empty,
      Map(HeightLockOneSpendingAddress -> HeightLockOneLock)
    )

  /**
   * Incorporate a Transaction into a Wallet by removing spent outputs and including new outputs.
   */
  def applyTransaction(wallet: Wallet)(transaction: IoTransaction): Wallet = {
    val spentBoxIds = transaction.inputs.map(_.address)

    val transactionId = transaction.id
    val newBoxes =
      transaction.outputs.zipWithIndex.filter(_._1.value.value.isLvl).flatMap { case (output, index) =>
        wallet.propositions
          .get(output.address)
          .map(lock =>
            (
              // transactionId.outputAddress(0, 0, index),
              transactionId
                .outputAddress(
                  wallet.spendableBoxes.map(_._1.network).headOption.getOrElse(NetworkConstants.PRIVATE_NETWORK_ID),
                  wallet.spendableBoxes.map(_._1.ledger).headOption.getOrElse(NetworkConstants.MAIN_LEDGER_ID),
                  index
                ),
              Box(lock, output.value)
            )
          )
      }
    wallet.copy(spendableBoxes = wallet.spendableBoxes -- spentBoxIds ++ newBoxes)
  }
}
