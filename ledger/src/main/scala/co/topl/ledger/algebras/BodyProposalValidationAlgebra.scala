package co.topl.ledger.algebras

import co.topl.algebras.ContextualValidationAlgebra
import co.topl.node.models.BlockBody
import co.topl.ledger.models._

trait BodyProposalValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, BodyValidationError, BlockBody, BodyProposalValidationContext]
