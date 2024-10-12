package xyz.stratalab.ledger.algebras

import co.topl.brambl.models.Datum
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.node.models.BlockBody
import xyz.stratalab.quivr.runtime.DynamicContext
import xyz.stratalab.algebras.ContextualValidationAlgebra
import xyz.stratalab.ledger.models.BodyAuthorizationError

trait BodyAuthorizationValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[
      F,
      BodyAuthorizationError,
      BlockBody,
      IoTransaction => DynamicContext[F, String, Datum]
    ]
