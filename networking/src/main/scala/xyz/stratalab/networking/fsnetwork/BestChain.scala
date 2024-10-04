package xyz.stratalab.networking.fsnetwork

import cats.data._
import cats.implicits.catsSyntaxEq
import xyz.stratalab.consensus.models.{BlockId, SlotData}
import xyz.stratalab.typeclasses.implicits._

case class BestChain(slotData: NonEmptyChain[SlotData]) {
  val last: SlotData = slotData.last
  val lastId: BlockId = last.slotId.blockId

  def isLastId(id: BlockId): Boolean = lastId === id
}
