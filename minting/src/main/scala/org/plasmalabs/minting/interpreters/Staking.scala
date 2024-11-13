package org.plasmalabs.minting.interpreters

import cats.data.OptionT
import cats.effect._
import cats.implicits._
import com.google.protobuf.ByteString
import org.plasmalabs.algebras.Stats
import org.plasmalabs.catsutils._
import org.plasmalabs.codecs.bytes.tetra.instances._
import org.plasmalabs.codecs.bytes.typeclasses.implicits._
import org.plasmalabs.consensus.algebras._
import org.plasmalabs.consensus.models._
import org.plasmalabs.consensus.thresholdEvidence
import org.plasmalabs.crypto.hash.Blake2b256
import org.plasmalabs.crypto.signing.Ed25519
import org.plasmalabs.minting.algebras._
import org.plasmalabs.minting.models.VrfHit
import org.plasmalabs.models._
import org.plasmalabs.sdk.models.LockAddress
import org.plasmalabs.typeclasses.implicits._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}

object Staking {

  // scalastyle:off method.length
  def make[F[_]: Async: Stats](
    a:                        StakingAddress,
    rewardAddress:            LockAddress,
    vkVrf:                    ByteString,
    operationalKeyMaker:      OperationalKeyMakerAlgebra[F],
    consensusState:           ConsensusValidationStateAlgebra[F],
    etaCalculation:           EtaCalculationAlgebra[F],
    ed25519Resource:          Resource[F, Ed25519],
    blake2b256Resource:       Resource[F, Blake2b256],
    vrfCalculator:            VrfCalculatorAlgebra[F],
    leaderElectionValidation: LeaderElectionValidationAlgebra[F]
  ): Resource[F, StakingAlgebra[F]] =
    Resource
      .pure {
        val _rewardAddress = rewardAddress
        new StakingAlgebra[F] {

          implicit private val logger: SelfAwareStructuredLogger[F] =
            Slf4jLogger.getLoggerFromName[F]("Node.Staking")
          val address: F[StakingAddress] = a.pure[F]
          val rewardAddress: F[LockAddress] = _rewardAddress.pure[F]

          def elect(parentSlotId: SlotId, slot: Slot): F[Option[VrfHit]] = (
            for {
              eta <- OptionT.liftF(etaCalculation.etaToBe(parentSlotId, slot))
              relativeStake <- OptionT(
                consensusState
                  .operatorRelativeStake(parentSlotId.blockId, slot)(a)
                  .warnIfSlow("Local Operator Relative Stake")
              ).flatTapNone(
                Logger[F].debug(s"No stake at slot=$slot")
              )
              threshold <- OptionT.liftF(
                leaderElectionValidation
                  .getThreshold(relativeStake, slot - parentSlotId.slot)
                  .warnIfSlow("Local threshold")
              )
              testProof <- OptionT.liftF(vrfCalculator.proofForSlot(slot, eta))
              rho       <- OptionT.liftF(vrfCalculator.rhoForSlot(slot, eta))
              isLeader <- OptionT.liftF(
                leaderElectionValidation
                  .isSlotLeaderForThreshold(threshold)(rho)
                  .warnIfSlow("Local Operator Is Slot Leader")
              )
              logMessage = show"Eligibility at" +
                show" slot=$slot" +
                show" parentId=${parentSlotId.blockId}" +
                show" parentSlot=${parentSlotId.slot}" +
                show" eligible=$isLeader" +
                show" relativeStake=$relativeStake" +
                show" stakingAddress=$a"
              _ <- OptionT.liftF(
                Stats[F].recordGauge(
                  "plasma_node_staking_is_eligible",
                  "Boolean indicating if the staker is eligible in the current operational period.",
                  Map(),
                  longToJson(if (isLeader) 1L else 0L)
                )
              )
              _ <- OptionT.liftF(
                Stats[F].recordGauge(
                  "plasma_node_staking_relative_stake",
                  "Percentage of stake owned by the operator at the given slot.",
                  Map(),
                  longToJson((relativeStake.numerator / relativeStake.denominator).toLong)
                )
              )
              _ <- OptionT.liftF(if (isLeader) Logger[F].info(logMessage) else Logger[F].debug(logMessage))
              vrfHit <- OptionT
                .whenF[F, VrfHit](isLeader)(
                  blake2b256Resource
                    .use(implicit b => Sync[F].delay(thresholdEvidence(threshold)))
                    .map(evidence =>
                      VrfHit(
                        EligibilityCertificate(testProof, vkVrf, evidence, eta.data),
                        slot,
                        threshold
                      )
                    )
                )
            } yield vrfHit
          ).value

          def certifyBlock(
            parentSlotId:         SlotId,
            slot:                 Slot,
            unsignedBlockBuilder: UnsignedBlockHeader.PartialOperationalCertificate => UnsignedBlockHeader,
            eta:                  Eta
          ): F[Option[BlockHeader]] =
            OptionT(operationalKeyMaker.operationalKeyForSlot(slot, parentSlotId, eta)).semiflatMap {
              operationalKeyOut =>
                for {
                  partialCertificate <- Sync[F].delay(
                    UnsignedBlockHeader.PartialOperationalCertificate(
                      operationalKeyOut.parentVK,
                      operationalKeyOut.parentSignature,
                      operationalKeyOut.childVK
                    )
                  )
                  unsignedBlock = unsignedBlockBuilder(partialCertificate)
                  messageToSign = unsignedBlock.signableBytes.toByteArray
                  _ <- Async[F].cede
                  signature <- ed25519Resource.use(ed25519 =>
                    Sync[F].delay(
                      ed25519.sign(
                        Ed25519.SecretKey(operationalKeyOut.childSK.toByteArray),
                        messageToSign
                      )
                    )
                  )
                  _ <- Async[F].cede
                  operationalCertificate = OperationalCertificate(
                    operationalKeyOut.parentVK,
                    operationalKeyOut.parentSignature,
                    partialCertificate.childVK,
                    ByteString.copyFrom(signature)
                  )
                  header = BlockHeader(
                    headerId = None,
                    unsignedBlock.parentHeaderId,
                    unsignedBlock.parentSlot,
                    unsignedBlock.txRoot,
                    unsignedBlock.bloomFilter,
                    unsignedBlock.timestamp,
                    unsignedBlock.height,
                    unsignedBlock.slot,
                    unsignedBlock.eligibilityCertificate,
                    operationalCertificate,
                    unsignedBlock.metadata,
                    unsignedBlock.address,
                    unsignedBlock.protocolVersion
                  )
                } yield header
            }.value
        }
      }
  // scalastyle:on method.length
}
