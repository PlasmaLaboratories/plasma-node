package xyz.stratalab.ledger.algebras

import co.topl.node.models.BlockBody
import xyz.stratalab.algebras.ContextualValidationAlgebra
import xyz.stratalab.ledger.models._

trait BodyProposalValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, BodyValidationError, BlockBody, BodyProposalValidationContext]
