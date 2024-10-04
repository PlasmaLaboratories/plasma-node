package xyz.stratalab.ledger.algebras

import xyz.stratalab.algebras.ContextualValidationAlgebra
import xyz.stratalab.ledger.models.{BodySemanticError, BodyValidationContext}
import xyz.stratalab.node.models.BlockBody

trait BodySemanticValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, BodySemanticError, BlockBody, BodyValidationContext]
