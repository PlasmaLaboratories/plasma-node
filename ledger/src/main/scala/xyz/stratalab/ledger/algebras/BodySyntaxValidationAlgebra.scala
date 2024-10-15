package xyz.stratalab.ledger.algebras

import xyz.stratalab.algebras.ContextlessValidationAlgebra
import xyz.stratalab.ledger.models.BodySyntaxError
import xyz.stratalab.node.models.BlockBody

trait BodySyntaxValidationAlgebra[F[_]] extends ContextlessValidationAlgebra[F, BodySyntaxError, BlockBody]
