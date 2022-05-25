package co.topl.ledger.interpreters

import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.effect.Sync
import cats.implicits._
import co.topl.ledger.algebras.{InvalidSyntaxError, InvalidSyntaxErrors, SyntacticValidationAlgebra}
import co.topl.models.{Box, Transaction}
import co.topl.typeclasses.implicits._

object SyntacticValidation {

  def make[F[_]: Sync]: F[SyntacticValidationAlgebra[F]] =
    Sync[F].delay(
      (
        transaction => Sync[F].delay(validators.foldMap(_.apply(transaction)).as(transaction))
      ): SyntacticValidationAlgebra[F]
    )

  private[interpreters] val validators: Chain[Transaction => ValidatedNec[InvalidSyntaxError, Unit]] =
    Chain(
      nonEmptyInputsValidation,
      positiveTimestampValidation,
      positiveOutputValuesValidation,
      sufficientFundsValidation
    )

  /**
   * Verify that this transaction contains at least one input
   */
  private[interpreters] def nonEmptyInputsValidation(transaction: Transaction): ValidatedNec[InvalidSyntaxError, Unit] =
    Validated.condNec(transaction.inputs.nonEmpty, (), InvalidSyntaxErrors.EmptyInputs)

  /**
   * Verify that the timestamp of the transaction is positive (greater than 0).  Transactions _can_ be created
   * in the past.
   */
  private[interpreters] def positiveTimestampValidation(
    transaction: Transaction
  ): ValidatedNec[InvalidSyntaxError, Unit] =
    Validated.condNec(transaction.timestamp >= 0, (), InvalidSyntaxErrors.InvalidTimestamp(transaction.timestamp))

  /**
   * Verify that each transaction output contains a positive quantity (where applicable)
   */
  private[interpreters] def positiveOutputValuesValidation(
    transaction: Transaction
  ): ValidatedNec[InvalidSyntaxError, Unit] =
    transaction.outputs
      .foldMap(output =>
        (output.value match {
          case t: Box.Values.Poly  => t.quantity.data.some
          case t: Box.Values.Arbit => t.quantity.data.some
          case t: Box.Values.Asset => t.quantity.data.some
          case _                   => none
        }).foldMap(quantity =>
          Validated
            .condNec(
              quantity > BigInt(0),
              (),
              InvalidSyntaxErrors.NonPositiveOutputValue(output.value): InvalidSyntaxError
            )
        )
      )

  /**
   * Ensure the input value quantities exceed or equal the (non-minting) output value quantities
   */
  private[interpreters] def sufficientFundsValidation(
    transaction: Transaction
  ): ValidatedNec[InvalidSyntaxError, Unit] =
    quantityBasedValidation(transaction) { f =>
      val filteredInputs = transaction.inputs.map(_.value).filter(f.isDefinedAt)
      val filteredOutputs = transaction.outputs.filterNot(_.minting).map(_.value).filter(f.isDefinedAt)
      val inputsSum = filteredInputs.map(f).sumAll
      val outputsSum = filteredOutputs.map(f).sumAll
      Validated.condNec(
        inputsSum >= outputsSum,
        (),
        InvalidSyntaxErrors.InsufficientInputFunds(filteredInputs, filteredOutputs): InvalidSyntaxError
      )
    }

  /**
   * Perform validation based on the quantities of boxes grouped by type
   * @param f an extractor function which retrieves a BigInt from a Box.Value
   */
  private[interpreters] def quantityBasedValidation(transaction: Transaction)(
    f: PartialFunction[Box.Value, BigInt] => ValidatedNec[InvalidSyntaxError, Unit]
  ): ValidatedNec[InvalidSyntaxError, Unit] =
    NonEmptyChain(
      // Extract all Poly values and their quantities
      f { case Box.Values.Poly(quantity) => quantity.data },
      // Extract all Asset values and their quantities
      f { case Box.Values.Arbit(quantity) => quantity.data }
    ).appendChain(
      // Extract all Asset values (grouped by asset code) and their quantities
      Chain.fromSeq(
        (transaction.inputs.map(_.value) ++ transaction.outputs.map(_.value))
          .collect { case a: Box.Values.Asset =>
            a.assetCode
          }
          .toList
          .distinct
          .map(code => f { case a: Box.Values.Asset if a.assetCode === code => a.quantity.data })
      )
    ).combineAll
}
