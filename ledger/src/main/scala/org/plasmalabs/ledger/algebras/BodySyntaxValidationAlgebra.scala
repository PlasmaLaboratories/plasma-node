package org.plasmalabs.ledger.algebras

import org.plasmalabs.algebras.ContextlessValidationAlgebra
import org.plasmalabs.ledger.models.BodySyntaxError
import org.plasmalabs.node.models.BlockBody

trait BodySyntaxValidationAlgebra[F[_]] extends ContextlessValidationAlgebra[F, BodySyntaxError, BlockBody]
