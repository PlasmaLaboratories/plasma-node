package org.plasmalabs.ledger.implicits

import cats.Show
import cats.implicits._
import org.plasmalabs.ledger.models._
import org.plasmalabs.quivr.runtime.QuivrRuntimeError
import org.plasmalabs.sdk.syntax._
import org.plasmalabs.sdk.validation.{TransactionAuthorizationError, TransactionSyntaxError}
import org.plasmalabs.typeclasses.implicits._

trait LedgerShowInstances {

  implicit val showTransactionSyntaxError: Show[TransactionSyntaxError] =
    _.getClass.getName

  implicit val showTransactionSemanticError: Show[TransactionSemanticError] =
    _.getClass.getName

  implicit val showQuivrRuntimeError: Show[QuivrRuntimeError] =
    _.getClass.getName

  implicit val showAuthorizationError: Show[TransactionAuthorizationError] = {
    case TransactionAuthorizationError.AuthorizationFailed(errors) => errors.mkString_("[", ", ", "]")
    case TransactionAuthorizationError.Contextual(error)           => show"Contextual($error)"
    case TransactionAuthorizationError.Permanent(error)            => show"Permanent($error)"
  }

  implicit val showBodySyntaxError: Show[BodySyntaxError] = {
    case BodySyntaxErrors.TransactionSyntaxErrors(t, e) =>
      show"TransactionSyntaxErrors(${t.id}, $e)"
    case BodySyntaxErrors.DoubleSpend(_) =>
      show"DoubleSpend"
    case BodySyntaxErrors.InvalidReward(_) =>
      show"InvalidReward"
  }

  implicit val showBodySemanticError: Show[BodySemanticError] = {
    case BodySemanticErrors.TransactionSemanticErrors(t, e) =>
      show"TransactionSemanticError(${t.id}, $e)"
    case BodySemanticErrors.TransactionRegistrationError(_) =>
      "TransactionRegistrationError"
    case BodySemanticErrors.RewardTransactionError(_) =>
      "RewardTransactionError"
  }

  implicit val showBodyAuthorizationError: Show[BodyAuthorizationError] = {
    case BodyAuthorizationErrors.TransactionAuthorizationErrors(t, e) =>
      show"TransactionAuthorizationError(${t.id}, $e)"
  }

  implicit val showBodyValidationError: Show[BodyValidationError] = {
    case e: BodySyntaxError        => e.show
    case e: BodySemanticError      => e.show
    case e: BodyAuthorizationError => e.show
  }

}
