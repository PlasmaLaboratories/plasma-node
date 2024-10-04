package xyz.stratalab.ledger.interpreters

import cats.effect.Sync
import cats.implicits._
import xyz.stratalab.consensus.models.BlockHeader
import xyz.stratalab.models.Slot
import xyz.stratalab.quivr.runtime.DynamicContext
import xyz.stratalab.sdk.Context
import xyz.stratalab.sdk.models._
import xyz.stratalab.sdk.models.transaction.IoTransaction

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
