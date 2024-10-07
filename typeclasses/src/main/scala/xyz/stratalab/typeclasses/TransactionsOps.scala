package xyz.stratalab.typeclasses

import xyz.stratalab.sdk.models.box.Value.ConfigProposal
import xyz.stratalab.sdk.models.transaction.IoTransaction

trait TransactionsOps {

  implicit class TransactionsOps(txs: Seq[IoTransaction]) {
    def proposals: List[ConfigProposal] = txs.flatMap(_.outputs).map(_.value.value).flatMap(_.configProposal).toList
  }
}

object TransactionsOps extends TransactionsOps
