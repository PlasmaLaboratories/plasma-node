package co.topl.typeclasses

import cats.Show
import cats.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.Identifiable
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models._
import co.topl.models.utility._

import co.topl.consensus.{models => consensusModels}
import com.google.protobuf.ByteString
import java.time.Instant

trait ShowInstances {

  implicit def showInstanceFromIdentifiable[T: Identifiable]: Show[T] =
    t => {
      val (prefix, bytes) = t.id
      show"($prefix)$bytes"
    }

  implicit val showBytes: Show[Bytes] =
    bytes => bytes.toBase58

  implicit val showByteString: Show[ByteString] =
    bytes => bytes.toBase58

  implicit def showSizedBytes[Data: Show, L <: Length](implicit l: L): Show[Sized.Strict[Data, L]] =
    sized => show"[${l.value}](${sized.data})"

  implicit val showIoTransaction32Id: Show[co.topl.brambl.models.Identifier.IoTransaction32] =
    t => show"t_${t.evidence.digest.value: Bytes}"

  implicit val showBlockId: Show[co.topl.consensus.models.BlockId] =
    b => show"b_${b.value: Bytes}"

  implicit val showTypedIdentifier: Show[TypedIdentifier] =
    id =>
      id.typePrefix match {
        case IdentifierTypes.Block.HeaderV2 =>
          show"b_${id.dataBytes}"
        case IdentifierTypes.Transaction =>
          show"t_${id.dataBytes}"
        case p =>
          show"c${p}_${id.dataBytes}"
      }

  implicit val showSlotId: Show[SlotId] =
    slotID => show"{${slotID.slot},${slotID.blockId}}"

  implicit val showConsensusSlotId: Show[consensusModels.SlotId] =
    slotID => show"{${slotID.slot},${slotID.blockId}}"

  implicit val showRho: Show[Rho] =
    _.sizedBytes.show

  implicit val showStakingAddressesOperator: Show[StakingAddresses.Operator] =
    showBytes.contramap[StakingAddresses.Operator](_.immutableBytes)

  implicit val showStakingAddress: Show[StakingAddress] =
    showBytes.contramap[StakingAddress](_.immutableBytes)

  import IdentityOps._

  implicit val showBlockHeader: Show[BlockHeader] =
    header =>
      show"BlockHeader(" +
      show"id=${header.id.asTypedBytes}" +
      show" parentId=${header.parentHeaderId}" +
      show" parentSlot=${header.parentSlot}" +
      show" timestamp=${Instant.ofEpochMilli(header.timestamp).toString})" +
      show" height=${header.height}" +
      show" slot=${header.slot}" +
      show" txRoot=${header.txRoot.data}" +
      show" bloomFilter=${header.bloomFilter.data}" +
      show" eligibilityCertificate=${header.eligibilityCertificate.immutableBytes}" +
      show" operationalCertificate=${header.operationalCertificate.immutableBytes}" +
      show" address=${header.address: StakingAddress}" +
      show")"

  implicit val showConsensusBlockHeader: Show[consensusModels.BlockHeader] =
    header =>
      show"BlockHeader(id=${header.id.asTypedBytes}" +
      show" parentId=${(header.parentHeaderId: TypedIdentifier)}" +
      show" height=${header.height}" +
      show" slot=${header.slot}" +
      show" timestamp=${Instant.ofEpochMilli(header.timestamp).toString})" +
      show" address=${co.topl.models.StakingAddresses.operatorFromProtoString(header.address).show}"

  implicit val showNodeBlockBody: Show[co.topl.node.models.BlockBody] =
    body => show"${body.transactionIds.map(t => t: TypedIdentifier)}"

  implicit val showBoxId: Show[Box.Id] =
    boxId => show"${boxId.transactionId}.outputs[${boxId.transactionOutputIndex}]"
}

object ShowInstances extends ShowInstances