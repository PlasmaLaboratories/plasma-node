package org.plasmalabs.ledger.interpreters

import cats.effect.*
import org.plasmalabs.ledger.algebras.TransactionRewardCalculatorAlgebra
import org.plasmalabs.ledger.models.{AssetId, RewardQuantities}
import org.plasmalabs.sdk.models.box.Value
import org.plasmalabs.sdk.models.transaction.IoTransaction

/**
 * Implements a TransactionRewardCalculator which groups fungible token types and subtracts their output quantities
 * from the input quantities.
 */
object TransactionRewardCalculator {

  def make[F[_]]: Resource[F, TransactionRewardCalculatorAlgebra] =
    Resource.pure(tx =>
      RewardQuantities(
        (sumLvls(tx.inputs)(_.value) - sumLvls(tx.outputs)(_.value)).max(BigInt(0)),
        (sumTopls(tx.inputs)(_.value) - sumTopls(tx.outputs)(_.value)).max(BigInt(0)),
        diffAssets(tx)
      )
    )

  /**
   * Extracts LVL Box Values from the given collection, and sums the quantities
   * @param containsValues a collection that contains some value T
   * @param extractValue a function to extract a Value from T
   * @tparam T an abstract type (SpentTransactionOutput or UnspentTransactionOutput)
   * @return a BigInt sum
   */
  def sumLvls[T](containsValues: Iterable[T])(extractValue: T => Value): BigInt =
    containsValues
      .map(extractValue)
      .flatMap(_.value.lvl)
      .map(_.quantity.value.toByteArray)
      .map(BigInt(_))
      .sum

  /**
   * Extracts Topl Box Values from the given collection, and sums the quantities
   * @param containsValues a collection that contains some value T
   * @param extractValue a function to extract a Value from T
   * @tparam T an abstract type (SpentTransactionOutput or UnspentTransactionOutput)
   * @return a BigInt sum
   */
  def sumTopls[T](containsValues: Iterable[T])(extractValue: T => Value): BigInt =
    containsValues
      .map(extractValue)
      .flatMap(_.value.topl)
      .map(_.quantity.value.toByteArray)
      .map(BigInt(_))
      .sum

  /**
   * Extracts Asset Box Values from the given collection, and sums the quantities based on their respective fungibility
   * @param containsValues a collection that contains some value T
   * @param extractValue a function to extract a Value from T
   * @tparam T an abstract type (SpentTransactionOutput or UnspentTransactionOutput)
   * @return a mapping from AssetId to BigInt sum
   */
  def sumAssets[T](containsValues: Iterable[T])(extractValue: T => Value): Map[AssetId, BigInt] =
    containsValues
      .map(extractValue)
      .flatMap(_.value.asset)
      .map(asset =>
        AssetId(
          asset.groupId,
          asset.seriesId,
          asset.groupAlloy,
          asset.seriesAlloy,
          asset.fungibility,
          asset.quantityDescriptor
        ) ->
        BigInt(asset.quantity.value.toByteArray)
      )
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).sum)
      .toMap

  /**
   * Determines the excess assets of the given transactions
   * @return a mapping from AssetId to BigInt sum
   */
  def diffAssets(transaction: IoTransaction): Map[AssetId, BigInt] = {
    val in = sumAssets(transaction.inputs)(_.value)
    val out = sumAssets(transaction.outputs)(_.value)
    out.foldLeft(in) { case (result, (assetId, quantity)) =>
      result.updatedWith(assetId)(_.map(_ - quantity).filter(_ > 0))
    }
  }

}
