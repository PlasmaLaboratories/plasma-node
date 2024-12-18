package org.plasmalabs.typeclasses

import cats.Show
import cats.implicits.*
import com.google.protobuf.ByteString
import org.plasmalabs.codecs.bytes.tetra.instances.*
import org.plasmalabs.consensus.models.{BlockHeader, SlotId, StakingAddress}
import org.plasmalabs.models.*
import org.plasmalabs.models.utility.*
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.quivr.models.Int128
import org.plasmalabs.sdk.models.box.{Box, Value}
import org.plasmalabs.sdk.models.transaction.{SpentTransactionOutput, UnspentTransactionOutput}
import org.plasmalabs.sdk.models.{GroupId, SeriesId, TransactionId, TransactionOutputAddress}

import java.time.Instant

trait ShowInstances {

  implicit val showByteString: Show[ByteString] =
    bytes => bytes.toBase58

  implicit def showSizedBytes[Data: Show, L <: Length](implicit l: L): Show[Sized.Strict[Data, L]] =
    sized => show"[${l.value}](${sized.data})"

  implicit val showIoTransactionId: Show[TransactionId] =
    t => show"t_${t.value: Bytes}"

  implicit val showBlockId: Show[org.plasmalabs.consensus.models.BlockId] =
    b => show"b_${b.value: Bytes}"

  implicit val showConsensusSlotId: Show[SlotId] =
    slotID => show"{${slotID.slot},${slotID.blockId}}"

  implicit val showRho: Show[Rho] =
    _.sizedBytes.data.show

  implicit val showStakingAddress: Show[StakingAddress] =
    _.value.show

  implicit val showBlockHeader: Show[BlockHeader] =
    header =>
      show"BlockHeader(" +
      show"id=${header.id}" +
      show" parentId=${header.parentHeaderId}" +
      show" parentSlot=${header.parentSlot}" +
      show" timestamp=${Instant.ofEpochMilli(header.timestamp).toString}" +
      show" height=${header.height}" +
      show" slot=${header.slot}" +
      // Don't show these fields because they create too much noise in the logs
      // show" txRoot=${header.txRoot}" +
      // show" bloomFilter=${header.bloomFilter}" +
      // show" eligibilityCertificate=${header.eligibilityCertificate.toByteString}" +
      // show" operationalCertificate=${header.operationalCertificate.toByteString}" +
      show" address=${header.address}" +
      show")"

  implicit val showNodeBlockBody: Show[BlockBody] =
    body =>
      show"Body(transactionIds=[${body.transactionIds.length}]${body.transactionIds}, reward=${body.rewardTransactionId})"

  implicit val showBoxId: Show[TransactionOutputAddress] =
    boxId => show"${boxId.id}.outputs[${boxId.index}]"

  implicit val showGroupId: Show[GroupId] =
    g => show"g_${g.value: Bytes}"

  implicit val showSeriesId: Show[SeriesId] =
    s => show"s_${s.value: Bytes}"

  implicit val showInt128: Show[Int128] =
    v => BigInt(v.value.toByteArray).toString()

  implicit val showValue: Show[Value] =
    value =>
      value.value match {
        case Value.Value.Lvl(lvl)   => show"LVL(${lvl.quantity})"
        case Value.Value.Topl(topl) => show"TOPL(${topl.quantity})"
        case v                      => v.getClass.getName
      }

  implicit val showStxo: Show[SpentTransactionOutput] =
    stxo => show"Stxo(utxo=${stxo.address}, value=${stxo.value})"

  implicit val showUtxo: Show[UnspentTransactionOutput] =
    utxo => show"Utxo(value=${utxo.value})"

  implicit val showBox: Show[Box] =
    box => show"Box(value=${box.value})"

  implicit val showRatio: Show[org.plasmalabs.models.utility.Ratio] =
    r => s"${r.numerator}/${r.denominator}"

}

object ShowInstances extends ShowInstances
