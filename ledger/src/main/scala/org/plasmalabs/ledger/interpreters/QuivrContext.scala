package org.plasmalabs.ledger.interpreters

import cats.effect.Sync
import cats.implicits.*
import org.plasmalabs.consensus.models.BlockHeader
import org.plasmalabs.models.Slot
import org.plasmalabs.quivr.runtime.DynamicContext
import org.plasmalabs.sdk.Context
import org.plasmalabs.sdk.models.*
import org.plasmalabs.sdk.models.transaction.IoTransaction

object QuivrContext {

  def forConstructedBlock[F[_]: Sync](
    header:      BlockHeader,
    transaction: IoTransaction
  ): DynamicContext[F, String, Datum] =
    Context(
      transaction,
      header.slot,
      {
        case "header" => Datum().withHeader(Datum.Header(Event.Header(header.height))).some
        case _        => None
      }
    )

  def forProposedBlock[F[_]: Sync](
    height:      Long,
    slot:        Slot,
    transaction: IoTransaction
  ): DynamicContext[F, String, Datum] =
    Context(
      transaction,
      slot,
      {
        case "header" =>
          Datum().withHeader(Datum.Header(Event.Header(height))).some
        case _ =>
          None
      }
    )
}
