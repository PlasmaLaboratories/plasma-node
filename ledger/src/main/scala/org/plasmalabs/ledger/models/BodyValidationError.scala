package org.plasmalabs.ledger.models

import cats.data.{NonEmptyChain, NonEmptySet}
import org.plasmalabs.models.ProposalId
import org.plasmalabs.sdk.models.TransactionOutputAddress
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.validation.{TransactionAuthorizationError, TransactionSyntaxError}

sealed abstract class BodyValidationError

sealed abstract class BodyAuthorizationError extends BodyValidationError

object BodyAuthorizationErrors {

  case class TransactionAuthorizationErrors(
    transaction:        IoTransaction,
    authorizationError: TransactionAuthorizationError
  ) extends BodyAuthorizationError
}

sealed abstract class BodySemanticError extends BodyValidationError

object BodySemanticErrors {

  case class TransactionSemanticErrors(
    transaction:    IoTransaction,
    semanticErrors: NonEmptyChain[TransactionSemanticError]
  ) extends BodySemanticError

  case class TransactionRegistrationError(transaction: IoTransaction) extends BodySemanticError

  case class RewardTransactionError(transaction: IoTransaction) extends BodySemanticError

  case class ProposalTransactionAlreadyUsedId(proposalId: ProposalId) extends BodySemanticError

  case object DoubleProposalIdTransaction extends BodySemanticError
}

sealed trait BodySyntaxError extends BodyValidationError

object BodySyntaxErrors {

  case class TransactionSyntaxErrors(
    transaction:    IoTransaction,
    semanticErrors: NonEmptyChain[TransactionSyntaxError]
  ) extends BodySyntaxError

  case class DoubleSpend(boxIds: NonEmptySet[TransactionOutputAddress]) extends BodySyntaxError

  case class InvalidReward(rewardTransaction: IoTransaction) extends BodySyntaxError
}
