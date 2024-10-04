package xyz.stratalab.ledger.algebras

import co.topl.node.models.BlockBody
import xyz.stratalab.algebras.ContextualValidationAlgebra
import xyz.stratalab.ledger.models.{BodySemanticError, BodyValidationContext}

trait BodySemanticValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, BodySemanticError, BlockBody, BodyValidationContext]
