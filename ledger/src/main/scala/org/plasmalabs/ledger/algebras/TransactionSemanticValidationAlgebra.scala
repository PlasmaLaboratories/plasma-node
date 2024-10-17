package org.plasmalabs.ledger.algebras

import org.plasmalabs.algebras.ContextualValidationAlgebra
import org.plasmalabs.ledger.models.{TransactionSemanticError, TransactionValidationContext}
import org.plasmalabs.sdk.models.transaction.IoTransaction

trait TransactionSemanticValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, TransactionSemanticError, IoTransaction, TransactionValidationContext]
