package co.topl.typeclasses

import co.topl.brambl.models.box.Value.UpdateProposal
import co.topl.brambl.models.transaction.IoTransaction

trait TransactionsOps {

  implicit class TransactionsOps(txs: Seq[IoTransaction]) {
    def proposals: List[UpdateProposal] = txs.flatMap(_.outputs).map(_.value.value).flatMap(_.updateProposal).toList
  }
}

object TransactionsOps extends TransactionsOps
