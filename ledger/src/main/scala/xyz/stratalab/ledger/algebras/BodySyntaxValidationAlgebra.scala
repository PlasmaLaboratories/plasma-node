package xyz.stratalab.ledger.algebras

import co.topl.node.models.BlockBody
import xyz.stratalab.algebras.ContextlessValidationAlgebra
import xyz.stratalab.ledger.models.BodySyntaxError

trait BodySyntaxValidationAlgebra[F[_]] extends ContextlessValidationAlgebra[F, BodySyntaxError, BlockBody]
