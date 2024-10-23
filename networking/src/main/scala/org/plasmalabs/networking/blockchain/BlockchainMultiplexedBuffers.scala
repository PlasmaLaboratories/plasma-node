package org.plasmalabs.networking.blockchain

import cats.effect.{Async, Resource}
import cats.implicits._
import org.plasmalabs.consensus.models._
import org.plasmalabs.networking.multiplexer.MultiplexedBuffer
import org.plasmalabs.node.models._
import org.plasmalabs.sdk.models.TransactionId
import org.plasmalabs.sdk.models.transaction.IoTransaction

/**
 * Holds several instances of MultiplexedBuffers, specific to blockchain purposes
 */
case class BlockchainMultiplexedBuffers[F[_]](
  knownHosts:           MultiplexedBuffer[F, CurrentKnownHostsReq, Option[CurrentKnownHostsRes]],
  remotePeerServer:     MultiplexedBuffer[F, Unit, Option[KnownHost]],
  blockAdoptions:       MultiplexedBuffer[F, Unit, BlockId],
  transactionAdoptions: MultiplexedBuffer[F, Unit, TransactionId],
  pingPong:             MultiplexedBuffer[F, PingMessage, Option[PongMessage]],
  blockIdAtHeight:      MultiplexedBuffer[F, Long, Option[BlockId]],
  blockIdAtDepth:       MultiplexedBuffer[F, Long, Option[BlockId]],
  slotData:             MultiplexedBuffer[F, BlockId, Option[SlotData]],
  headers:              MultiplexedBuffer[F, BlockId, Option[BlockHeader]],
  bodies:               MultiplexedBuffer[F, BlockId, Option[BlockBody]],
  transactions:         MultiplexedBuffer[F, TransactionId, Option[IoTransaction]],
  appLevel:             MultiplexedBuffer[F, Boolean, Unit],
  slotDataAndParents:   MultiplexedBuffer[F, (BlockId, BlockId), Option[List[SlotData]]]
)

object BlockchainMultiplexedBuffers {

  def make[F[_]: Async]: Resource[F, BlockchainMultiplexedBuffers[F]] =
    (
      MultiplexedBuffer.make[F, CurrentKnownHostsReq, Option[CurrentKnownHostsRes]],
      MultiplexedBuffer.make[F, Unit, Option[KnownHost]],
      MultiplexedBuffer.make[F, Unit, BlockId],
      MultiplexedBuffer.make[F, Unit, TransactionId],
      MultiplexedBuffer.make[F, PingMessage, Option[PongMessage]],
      MultiplexedBuffer.make[F, Long, Option[BlockId]],
      MultiplexedBuffer.make[F, Long, Option[BlockId]],
      MultiplexedBuffer.make[F, BlockId, Option[SlotData]],
      MultiplexedBuffer.make[F, BlockId, Option[BlockHeader]],
      MultiplexedBuffer.make[F, BlockId, Option[BlockBody]],
      MultiplexedBuffer.make[F, TransactionId, Option[IoTransaction]],
      MultiplexedBuffer.make[F, Boolean, Unit],
      MultiplexedBuffer.make[F, (BlockId, BlockId), Option[List[SlotData]]]
    ).mapN(BlockchainMultiplexedBuffers.apply[F])

}
