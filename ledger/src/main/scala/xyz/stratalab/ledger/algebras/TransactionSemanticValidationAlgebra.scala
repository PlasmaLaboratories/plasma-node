package xyz.stratalab.ledger.algebras

import xyz.stratalab.algebras.ContextualValidationAlgebra
import xyz.stratalab.ledger.models.{TransactionSemanticError, TransactionValidationContext}
import xyz.stratalab.sdk.models.transaction.IoTransaction

trait TransactionSemanticValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, TransactionSemanticError, IoTransaction, TransactionValidationContext]
