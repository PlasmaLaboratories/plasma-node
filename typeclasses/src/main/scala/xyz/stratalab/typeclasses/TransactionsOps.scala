package xyz.stratalab.typeclasses

import xyz.stratalab.sdk.models.box.Value.UpdateProposal
import xyz.stratalab.sdk.models.transaction.IoTransaction

trait TransactionsOps {

  implicit class TransactionsOps(txs: Seq[IoTransaction]) {
    def proposals: List[UpdateProposal] = txs.flatMap(_.outputs).map(_.value.value).flatMap(_.updateProposal).toList
  }
}

object TransactionsOps extends TransactionsOps
