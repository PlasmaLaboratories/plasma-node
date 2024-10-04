package xyz.stratalab.ledger.interpreters

import cats.data.ValidatedNec
import cats.effect.Sync
import cats.implicits._
import xyz.stratalab.ledger.algebras._
import xyz.stratalab.ledger.models._
import xyz.stratalab.node.models.BlockBody
import xyz.stratalab.quivr.runtime.DynamicContext
import xyz.stratalab.sdk.models.transaction.IoTransaction
import xyz.stratalab.sdk.models.{Datum, TransactionId}
import xyz.stratalab.sdk.validation.algebras.TransactionAuthorizationVerifier

object BodyAuthorizationValidation {

  def make[F[_]: Sync](
    fetchTransaction:                   TransactionId => F[IoTransaction],
    transactionAuthorizationValidation: TransactionAuthorizationVerifier[F]
  ): F[BodyAuthorizationValidationAlgebra[F]] =
    Sync[F].delay {
      new BodyAuthorizationValidationAlgebra[F] {

        /**
         * Perform authorization validation on each of the transactions in this block
         */
        def validate(
          context: IoTransaction => DynamicContext[F, String, Datum]
        )(body: BlockBody): F[ValidatedNec[BodyAuthorizationError, BlockBody]] =
          // Note: Do not run authorization validation on the reward transaction
          body.transactionIds
            .foldMapM(transactionId =>
              for {
                transaction <- fetchTransaction(transactionId)
                quivrContext = context(transaction)
                validationResult <- transactionAuthorizationValidation.validate(quivrContext)(transaction)
              } yield validationResult
                .leftMap(error =>
                  BodyAuthorizationErrors.TransactionAuthorizationErrors(transaction, error): BodyAuthorizationError
                )
                .toValidatedNec
                .void
            )
            .map(_.as(body))
      }
    }

}
