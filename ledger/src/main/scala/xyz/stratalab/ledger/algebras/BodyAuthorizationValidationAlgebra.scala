package xyz.stratalab.ledger.algebras

import xyz.stratalab.algebras.ContextualValidationAlgebra
import xyz.stratalab.ledger.models.BodyAuthorizationError
import xyz.stratalab.node.models.BlockBody
import xyz.stratalab.quivr.runtime.DynamicContext
import xyz.stratalab.sdk.models.Datum
import xyz.stratalab.sdk.models.transaction.IoTransaction

trait BodyAuthorizationValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[
      F,
      BodyAuthorizationError,
      BlockBody,
      IoTransaction => DynamicContext[F, String, Datum]
    ]
