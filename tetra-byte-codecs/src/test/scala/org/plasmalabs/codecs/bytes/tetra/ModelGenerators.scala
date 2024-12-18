package org.plasmalabs.codecs.bytes.tetra

import cats.data.NonEmptyChain
import org.plasmalabs.codecs.bytes.tetra.instances.*
import org.plasmalabs.consensus.models.{BlockHeader, ProtocolVersion, SlotData}
import org.plasmalabs.models.generators.consensus.ModelGenerators.*
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.sdk.generators.ModelGenerators.arbitraryIoTransaction
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.syntax.TransactionSyntax
import org.scalacheck.{Arbitrary, Gen}

import scala.annotation.tailrec

trait ModelGenerators extends TransactionSyntax {

  val headerWithZeroVersions: Gen[BlockHeader] =
    arbitraryHeader.arbitrary.map(h => h.copy(version = ProtocolVersion(0, 0, 0)))

  @tailrec
  private def addHeaderToChain(
    headers:       NonEmptyChain[BlockHeader],
    gen:           Gen[BlockHeader],
    slotIncrement: Long => Long,
    count:         Long
  ): NonEmptyChain[BlockHeader] =
    count match {
      case 0 => headers
      case _ =>
        val nextSlot = slotIncrement(headers.last.slot)
        addHeaderToChain(
          headers.append(
            gen.sample.get
              .copy(parentHeaderId = headers.last.id, height = headers.last.height + 1, slot = nextSlot)
              .embedId
          ),
          gen,
          slotIncrement,
          count - 1
        )
    }

  def arbitraryLinkedHeaderChainFor(
    sizeGen:       Gen[Long],
    parentId:      Option[BlockHeader] = None,
    slotIncrement: Long => Long = _ + 1
  ): Arbitrary[NonEmptyChain[BlockHeader]] =
    Arbitrary(
      for {
        size <- sizeGen
        root <- headerWithZeroVersions
        updatedRoot =
          parentId
            .fold(root.copy(height = 1, slot = 1, parentSlot = 0))(p =>
              root.copy(parentHeaderId = p.id, height = p.height + 1)
            )
            .embedId
      } yield addHeaderToChain(NonEmptyChain.one(updatedRoot), headerWithZeroVersions, slotIncrement, size)
    )

  implicit val arbitraryLinkedHeaderChain: Arbitrary[NonEmptyChain[SlotData]] =
    arbitraryLinkedSlotDataChainFor(Gen.posNum[Long])

  implicit val arbitraryTxAndBlock: Arbitrary[(IoTransaction, BlockBody)] =
    Arbitrary(
      for {
        tx <- arbitraryIoTransaction.arbitrary.map(_.embedId)
        // TODO: Reward
      } yield (tx, BlockBody(Seq(tx.id)))
    )

  private val maxTxsCount = 5

  implicit val arbitraryTxsAndBlock: Arbitrary[(Seq[IoTransaction], BlockBody)] =
    Arbitrary(
      for {
        txs <- Gen.listOfN(maxTxsCount, arbitraryIoTransaction.arbitrary.map(_.embedId))
        // TODO: Reward
      } yield (txs, BlockBody(txs.map(tx => tx.id)))
    )
}

object ModelGenerators extends ModelGenerators
