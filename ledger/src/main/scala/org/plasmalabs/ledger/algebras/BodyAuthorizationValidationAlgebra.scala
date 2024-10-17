package org.plasmalabs.ledger.algebras

import org.plasmalabs.algebras.ContextualValidationAlgebra
import org.plasmalabs.ledger.models.BodyAuthorizationError
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.quivr.runtime.DynamicContext
import org.plasmalabs.sdk.models.Datum
import org.plasmalabs.sdk.models.transaction.IoTransaction

trait BodyAuthorizationValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[
      F,
      BodyAuthorizationError,
      BlockBody,
      IoTransaction => DynamicContext[F, String, Datum]
    ]
