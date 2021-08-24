package co.topl.consensus

import co.topl.models._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._
import co.topl.typeclasses.StatefullyValidatable
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

object ConsensusStatefullyValidatable
    extends StatefullyValidatable[
      ConsensusValidation.State,
      BlockHeaderV2,
      ConsensusValidation.ValidatedBlockHeader,
      ConsensusValidation.Failure
    ] {

  override def validate(
    header: BlockHeaderV2,
    state:  ConsensusValidation.State
  ): Either[ConsensusValidation.Failure, ConsensusValidation.ValidatedBlockHeader] = {
    import ConsensusValidation.Failures._
    val parent = state.parentBlockHeader
    def test(f: => Boolean, invalid: => ConsensusValidation.Failure) =
      Either.cond(f, header, invalid)
    for {
      _ <- test(header.slot > parent.slot, NonForwardSlot(header.slot, parent.slot))
      _ <- test(header.timestamp > parent.timestamp, NonForwardTimestamp(header.timestamp, parent.timestamp))
      _ <- test(header.parentHeaderId == parent.id, ParentMismatch(header.parentHeaderId, parent.id))

      // TODO: Generate test value for party (unique address) and slot,
      // and then compare normalized test value to the threshold

      // Cryptographic soundness of the proofs
      _ <- vrfVerification(header, state)
      _ <- kesVerification(header, state)
    } yield ConsensusValidation.ValidatedBlockHeader(header)
  }

  /**
   * Requires the epoch nonce, thus stateful
   */
  private[consensus] def vrfVerification(
    header: BlockHeaderV2,
    state:  ConsensusValidation.State
  ): Either[ConsensusValidation.Failure, BlockHeaderV2] = // TODO
    Either.cond(
      // TODO: header.vrfCertificate.publicKey
      true,
      header,
      ConsensusValidation.Failures.InvalidVrfCertificate(header.vrfCertificate)
    )

  /**
   * Requires the epoch nonce, thus stateful
   */
  private[consensus] def kesVerification(
    header: BlockHeaderV2,
    state:  ConsensusValidation.State
  ): Either[ConsensusValidation.Failure, BlockHeaderV2] = // TODO
    Either.cond(
      true,
      header,
      ConsensusValidation.Failures.InvalidKesCertificate(header.kesCertificate)
    )
}

object ConsensusValidation {

  implicit val instance: ConsensusStatefullyValidatable.type = ConsensusStatefullyValidatable

  sealed abstract class Failure

  object Failures {
    case class NonForwardSlot(slot: Slot, parentSlot: Slot) extends Failure
    case class NonForwardTimestamp(timestamp: Timestamp, parentTimestamp: Timestamp) extends Failure
    case class ParentMismatch(expectedParentId: TypedIdentifier, parentId: TypedIdentifier) extends Failure
    case class InvalidVrfCertificate(vrfCertificate: VrfCertificate) extends Failure
    case class InvalidKesCertificate(kesCertificate: KesCertificate) extends Failure
  }

  trait State {
    def epochNonce: Nonce
    def totalStake: Int128
    def parentBlockHeader: BlockHeaderV2
    def stakeFor(address: Address): Option[Int128]
  }

  @newtype class ValidatedBlockHeader(val header: BlockHeaderV2)

  private[consensus] object ValidatedBlockHeader {

    def apply(blockHeaderV2: BlockHeaderV2): ValidatedBlockHeader =
      blockHeaderV2.coerce
  }

}
