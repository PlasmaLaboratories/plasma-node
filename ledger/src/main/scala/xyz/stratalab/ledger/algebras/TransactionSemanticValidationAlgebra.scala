package xyz.stratalab.ledger.algebras

import co.topl.brambl.models.transaction.IoTransaction
import xyz.stratalab.algebras.ContextualValidationAlgebra
import xyz.stratalab.ledger.models.{TransactionSemanticError, TransactionValidationContext}

trait TransactionSemanticValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, TransactionSemanticError, IoTransaction, TransactionValidationContext]
