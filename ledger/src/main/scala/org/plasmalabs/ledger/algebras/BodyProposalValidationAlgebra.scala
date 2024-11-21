package org.plasmalabs.ledger.algebras

import org.plasmalabs.algebras.ContextualValidationAlgebra
import org.plasmalabs.ledger.models.*
import org.plasmalabs.node.models.BlockBody

trait BodyProposalValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, BodyValidationError, BlockBody, BodyProposalValidationContext]
