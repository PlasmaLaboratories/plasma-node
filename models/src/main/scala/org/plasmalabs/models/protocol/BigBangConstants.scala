package org.plasmalabs.models.protocol

import com.google.protobuf.ByteString
import org.plasmalabs.consensus.models.BlockId
import org.plasmalabs.models.Slot

object BigBangConstants {
  val BigBangParentId: BlockId = BlockId(ByteString.copyFrom(Array.fill[Byte](32)(0)))
  val BigBangParentSlot: Slot = -1L
  val BigBangSlot: Slot = 0L
  val BigBangHeight: Long = 1L
}
