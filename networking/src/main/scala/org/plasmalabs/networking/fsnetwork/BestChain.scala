package org.plasmalabs.networking.fsnetwork

import cats.data._
import cats.implicits.catsSyntaxEq
import org.plasmalabs.consensus.models.{BlockId, SlotData}
import org.plasmalabs.typeclasses.implicits._

case class BestChain(slotData: NonEmptyChain[SlotData]) {
  val last: SlotData = slotData.last
  val lastId: BlockId = last.slotId.blockId

  def isLastId(id: BlockId): Boolean = lastId === id
}
