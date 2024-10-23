package org.plasmalabs.ledger.algebras

import org.plasmalabs.algebras.ContextualValidationAlgebra
import org.plasmalabs.ledger.models.{BodySemanticError, BodyValidationContext}
import org.plasmalabs.node.models.BlockBody

trait BodySemanticValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, BodySemanticError, BlockBody, BodyValidationContext]
