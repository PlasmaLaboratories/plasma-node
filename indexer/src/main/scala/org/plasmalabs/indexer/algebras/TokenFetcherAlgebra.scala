package org.plasmalabs.indexer.algebras

import org.plasmalabs.indexer.model.GE
import org.plasmalabs.sdk.models.Event.{GroupPolicy, SeriesPolicy}
import org.plasmalabs.sdk.models.{GroupId, SeriesId}

/**
 * Fetch Value information stored the data store.
 * Value: LVL, TOPL, Asset, Group, Series,..
 * @tparam F the effect-ful context to retrieve the value in
 */
trait TokenFetcherAlgebra[F[_]] {

  /**
   * Fetch Group Policy
   *
   * @param groupId groupId
   * @return a group policy
   */
  def fetchGroupPolicy(groupId: GroupId): F[Either[GE, Option[GroupPolicy]]]

  /**
   * Fetch Series Policy
   *
   * @param seriesId seriesId
   * @return a series policy
   */
  def fetchSeriesPolicy(seriesId: SeriesId): F[Either[GE, Option[SeriesPolicy]]]

}
