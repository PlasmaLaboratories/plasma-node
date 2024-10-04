package xyz.stratalab.ledger.algebras

import xyz.stratalab.algebras.ContextualValidationAlgebra
import xyz.stratalab.ledger.models._
import xyz.stratalab.node.models.BlockBody

trait BodyProposalValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, BodyValidationError, BlockBody, BodyProposalValidationContext]
