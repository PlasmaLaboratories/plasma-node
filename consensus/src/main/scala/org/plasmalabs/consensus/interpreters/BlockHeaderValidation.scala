package org.plasmalabs.consensus.interpreters

import cats.data.*
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits.*
import com.github.benmanes.caffeine.cache.Caffeine
import com.google.common.primitives.Longs
import com.google.protobuf.ByteString
import org.plasmalabs.algebras.ClockAlgebra.implicits.clockAsClockOps
import org.plasmalabs.algebras.{ClockAlgebra, Store}
import org.plasmalabs.codecs.bytes.tetra.instances.*
import org.plasmalabs.codecs.bytes.typeclasses.implicits.*
import org.plasmalabs.consensus.algebras.*
import org.plasmalabs.consensus.models.{BlockHeader, BlockId, SlotData, SlotId, *}
import org.plasmalabs.consensus.{thresholdEvidence, *}
import org.plasmalabs.crypto.hash.Blake2b256
import org.plasmalabs.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import org.plasmalabs.models.*
import org.plasmalabs.models.utility.*
import org.plasmalabs.models.utility.HasLength.instances.*
import org.plasmalabs.models.utility.Lengths.*
import org.plasmalabs.typeclasses.implicits.*
import scalacache.Entry
import scalacache.caffeine.CaffeineCache

/**
 * Interpreters for the ConsensusValidationAlgebra
 */
object BlockHeaderValidation {

  def make[F[_]: Async](
    etaInterpreter:               EtaCalculationAlgebra[F],
    consensusValidationState:     ConsensusValidationStateAlgebra[F],
    leaderElection:               LeaderElectionValidationAlgebra[F],
    blockHeaderVersionValidation: BlockHeaderVersionValidationAlgebra[F],
    blockHeaderVotingValidation:  BlockHeaderVotingValidationAlgebra[F],
    eligibilityCache:             EligibilityCacheAlgebra[F],
    clockAlgebra:                 ClockAlgebra[F],
    blockHeaderStore:             Store[F, BlockId, BlockHeader],
    bigBangBlockId:               BlockId,
    ed25519VRFResource:           Resource[F, Ed25519VRF],
    kesProductResource:           Resource[F, KesProduct],
    ed25519Resource:              Resource[F, Ed25519],
    blake2b256Resource:           Resource[F, Blake2b256]
  ): F[BlockHeaderValidationAlgebra[F]] =
    Async[F].delay(
      new Impl[F](
        etaInterpreter,
        consensusValidationState,
        leaderElection,
        blockHeaderVersionValidation,
        blockHeaderVotingValidation,
        eligibilityCache,
        clockAlgebra,
        blockHeaderStore,
        bigBangBlockId,
        ed25519VRFResource,
        kesProductResource,
        ed25519Resource,
        blake2b256Resource
      )
    )

  private class Impl[F[_]: Async](
    etaInterpreter:               EtaCalculationAlgebra[F],
    consensusValidationState:     ConsensusValidationStateAlgebra[F],
    leaderElection:               LeaderElectionValidationAlgebra[F],
    blockHeaderVersionValidation: BlockHeaderVersionValidationAlgebra[F],
    blockHeaderVotingValidation:  BlockHeaderVotingValidationAlgebra[F],
    eligibilityCache:             EligibilityCacheAlgebra[F],
    clockAlgebra:                 ClockAlgebra[F],
    blockHeaderStore:             Store[F, BlockId, BlockHeader],
    bigBangBlockId:               BlockId,
    ed25519VRFResource:           Resource[F, Ed25519VRF],
    kesProductResource:           Resource[F, KesProduct],
    ed25519Resource:              Resource[F, Ed25519],
    blake2b256Resource:           Resource[F, Blake2b256]
  ) extends BlockHeaderValidationAlgebra[F] {

    def couldBeValidated(header: BlockHeader, lastProcessedBodyInChain: SlotData): F[Boolean] =
      for {
        checkedHeaderEpoch <- clockAlgebra.epochOf(header.slot)
        bestBlockEpoch     <- clockAlgebra.epochOf(lastProcessedBodyInChain.slotId.slot)
      } yield (checkedHeaderEpoch - bestBlockEpoch) < 2

    def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]] = {
      if (header.id === bigBangBlockId) EitherT.rightT[F, BlockHeaderValidationFailure](header)
      else
        for {
          _         <- EitherT(blockHeaderVersionValidation.validate(header))
          parent    <- EitherT.liftF(blockHeaderStore.getOrRaise(header.parentHeaderId))
          _         <- EitherT.liftF(Async[F].cede)
          _         <- statelessVerification(header, parent)
          _         <- EitherT.liftF(Async[F].cede)
          _         <- EitherT(timeSlotVerification(header))
          _         <- vrfVerification(header)
          _         <- EitherT.liftF(Async[F].cede)
          _         <- kesVerification(header)
          _         <- EitherT.liftF(Async[F].cede)
          _         <- registrationVerification(header)
          _         <- EitherT.liftF(Async[F].cede)
          threshold <- vrfThresholdFor(header, parent)
          _         <- EitherT.liftF(Async[F].cede)
          _         <- vrfThresholdVerification(header, threshold, blake2b256Resource)
          _         <- EitherT.liftF(Async[F].cede)
          _         <- eligibilityVerification(header, threshold)
          _         <- EitherT(blockHeaderVotingValidation.validate(header))
        } yield header
    }.value

    private[consensus] def statelessVerification(child: BlockHeader, parent: BlockHeader) =
      for {
        _ <- EitherT
          .cond[F](child.slot > parent.slot, (), BlockHeaderValidationFailures.NonForwardSlot(child.slot, parent.slot))
        _ <- EitherT.cond[F](
          child.timestamp > parent.timestamp,
          (),
          BlockHeaderValidationFailures.NonForwardTimestamp(child.timestamp, parent.timestamp)
        )
        _ <- EitherT
          .cond[F](
            child.height === parent.height + 1,
            (),
            BlockHeaderValidationFailures.NonForwardHeight(child.height, parent.height)
          )
          .leftWiden[BlockHeaderValidationFailure]
      } yield child

    private[consensus] def timeSlotVerification(header: BlockHeader) =
      for {
        globalSlot              <- clockAlgebra.globalSlot
        childSlotFromTimestamp  <- clockAlgebra.timestampToSlot(header.timestamp)
        forwardBiasedSlotWindow <- clockAlgebra.forwardBiasedSlotWindow
      } yield Either
        .right[BlockHeaderValidationFailure, BlockHeader](header)
        .ensureOr(child => BlockHeaderValidationFailures.TimestampSlotMismatch(child.slot, child.timestamp))(child =>
          childSlotFromTimestamp === child.slot
        )
        .ensureOr(child => BlockHeaderValidationFailures.SlotBeyondForwardBiasedSlotWindow(globalSlot, child.slot))(
          child => child.slot < globalSlot + forwardBiasedSlotWindow
        )

    /**
     * Verifies the given block's VRF certificate syntactic integrity for a particular stateful nonce
     */
    private[consensus] def vrfVerification(
      header: BlockHeader
    ): EitherT[F, BlockHeaderValidationFailure, BlockHeader] =
      for {
        _ <- EitherT.liftF(Async[F].cede)
        expectedEta <- EitherT.liftF(
          etaInterpreter.etaToBe(
            SlotId(header.parentSlot, header.parentHeaderId),
            header.slot
          )
        )
        eta = Sized.strictUnsafe(header.eligibilityCertificate.eta): Eta
        _ <- EitherT.liftF(Async[F].cede)
        _ <- EitherT.cond[F](
          eta === expectedEta,
          (),
          BlockHeaderValidationFailures.InvalidEligibilityCertificateEta(eta, expectedEta)
        )
        _ <- EitherT.liftF(Async[F].cede)
        signatureVerificationResult <- EitherT.liftF(
          ed25519VRFResource
            .use(ed25519vrf =>
              Async[F].delay(
                ed25519vrf
                  .verify(
                    header.eligibilityCertificate.vrfSig.toByteArray,
                    VrfArgument(expectedEta, header.slot).signableBytes.toByteArray,
                    header.eligibilityCertificate.vrfVK.toByteArray
                  )
              )
            )
        )
        _ <- EitherT.liftF(Async[F].cede)
        _ <- EitherT
          .cond[F](
            signatureVerificationResult,
            (),
            BlockHeaderValidationFailures.InvalidEligibilityCertificateProof(header.eligibilityCertificate.vrfSig)
          )
          .leftWiden[BlockHeaderValidationFailure]
      } yield header

    /**
     * Verifies the given block's Operational Certificate's parent -> linear commitment, and the Operational
     * Certificate's block signature
     */
    private[consensus] def kesVerification(
      header: BlockHeader
    ): EitherT[F, BlockHeaderValidationFailure, BlockHeader] =
      for {
        message <- EitherT.liftF(
          Async[F].delay(
            header.operationalCertificate.childVK.toByteArray ++ Longs.toByteArray(header.slot)
          )
        )
        _ <- EitherT.liftF(Async[F].cede)
        parentCommitmentResult <- EitherT.liftF(
          kesProductResource
            .use(kesProduct =>
              Async[F].delay(
                kesProduct
                  .verify(
                    header.operationalCertificate.parentSignature,
                    message,
                    header.operationalCertificate.parentVK
                  )
              )
            )
        )
        _ <- EitherT.liftF(Async[F].cede)
        _ <- EitherT.cond[F](
          parentCommitmentResult,
          (),
          BlockHeaderValidationFailures.InvalidOperationalParentSignature(header.operationalCertificate)
        )
        _ <- EitherT.liftF(Async[F].cede)
        childSignatureResult <- EitherT.liftF(
          ed25519Resource
            .use(ed25519 =>
              // Use the ed25519 instance to verify the childSignature against the header's bytes
              Async[F].delay(
                ed25519
                  .verify(
                    header.operationalCertificate.childSignature.toByteArray,
                    header.unsigned.signableBytes.toByteArray,
                    Ed25519.PublicKey(header.operationalCertificate.childVK.toByteArray)
                  )
              )
            )
        )
        _ <- EitherT.liftF(Async[F].cede)
        _ <- EitherT
          .cond[F](
            childSignatureResult,
            (),
            BlockHeaderValidationFailures.InvalidBlockProof(header.operationalCertificate)
          )
          .leftWiden[BlockHeaderValidationFailure]
      } yield header

    /**
     * Determines the VRF threshold for the given child
     */
    private def vrfThresholdFor(
      child:  BlockHeader,
      parent: BlockHeader
    ): EitherT[F, BlockHeaderValidationFailure, Ratio] =
      EitherT
        .fromOptionF(
          consensusValidationState.operatorRelativeStake(child.id, child.slot)(child.address),
          BlockHeaderValidationFailures.Unregistered(child.address)
        )
        .leftWiden[BlockHeaderValidationFailure]
        .semiflatMap(leaderElection.getThreshold(_, child.slot - parent.slot))

    /**
     * Verify that the threshold evidence stamped on the block matches the threshold generated using local state
     */
    private[consensus] def vrfThresholdVerification(
      header:             BlockHeader,
      threshold:          Ratio,
      blake2b256Resource: Resource[F, Blake2b256]
    ): EitherT[F, BlockHeaderValidationFailure, BlockHeader] =
      for {
        evidence <-
          EitherT
            .liftF[F, BlockHeaderValidationFailure, ByteString](
              blake2b256Resource.use(b => Async[F].delay(thresholdEvidence(threshold)(b)))
            )
        _ <- EitherT.liftF(Async[F].cede)
        _ <-
          EitherT.cond[F](
            header.eligibilityCertificate.thresholdEvidence === evidence,
            header,
            BlockHeaderValidationFailures.InvalidVrfThreshold(threshold): BlockHeaderValidationFailure
          )
      } yield header

    /**
     * Verify that the block's staker is eligible using their relative stake distribution
     */
    private[consensus] def eligibilityVerification(
      header:    BlockHeader,
      threshold: Ratio
    ): EitherT[F, BlockHeaderValidationFailure, BlockHeader] =
      for {
        proofHashBytes <- EitherT
          .liftF(
            ed25519VRFResource
              .use(ed25519Vrf =>
                Async[F].delay(
                  ed25519Vrf.proofToHash(header.eligibilityCertificate.vrfSig.toByteArray)
                )
              )
          )
        rho = Rho(Sized.strictUnsafe(ByteString.copyFrom(proofHashBytes)))
        isSlotLeader <- EitherT.liftF(leaderElection.isSlotLeaderForThreshold(threshold)(rho))
        _ <- EitherT
          .cond[F](
            isSlotLeader,
            (),
            BlockHeaderValidationFailures.IneligibleCertificate(threshold, header.eligibilityCertificate)
          )
          .leftWiden[BlockHeaderValidationFailure]
        // Warning: This is most likely a side effecting operation
        isNewEligibility <- EitherT.liftF(
          eligibilityCache.tryInclude(header.id, header.eligibilityCertificate.vrfVK, header.slot)
        )
        _ <- EitherT
          .cond[F](
            isNewEligibility,
            (),
            BlockHeaderValidationFailures.DuplicateEligibility(header.eligibilityCertificate.vrfVK, header.slot)
          )
          .leftWiden[BlockHeaderValidationFailure]
      } yield header

    /**
     * Verifies the staker's registration.  First checks that the staker is registered at all.  Once retrieved,
     * the registration contains a commitment/proof that must be verified using
     * the 0th timestep of the header's operational certificate's "parentVK".  The proof's message is the hash of
     * (the staker's vrfVK concatenated with the staker's poolVK).
     */
    private[consensus] def registrationVerification(
      header: BlockHeader
    ): EitherT[F, BlockHeaderValidationFailure, BlockHeader] =
      for {
        _ <- EitherT.liftF(Async[F].cede)
        staker <-
          EitherT.fromOptionF(
            consensusValidationState.staker(header.id, header.slot)(header.address),
            BlockHeaderValidationFailures.Unregistered(header.address)
          )
        _ <- EitherT.liftF(Async[F].cede)
        message <- EitherT.liftF(
          blake2b256Resource
            .use(b =>
              Async[F].delay(
                b.hash(header.eligibilityCertificate.vrfVK, header.address.value)
              )
            )
        )
        _ <- EitherT.liftF(Async[F].cede)
        isValid <- EitherT.liftF(
          kesProductResource
            .use(p =>
              Async[F].delay(
                p.verify(staker.registration.signature, message, header.operationalCertificate.parentVK.copy(step = 0))
              )
            )
        )
        _ <- EitherT.liftF(Async[F].cede)
        _ <- EitherT
          .cond[F](
            isValid,
            (),
            BlockHeaderValidationFailures
              .RegistrationCommitmentMismatch(
                staker.registration.signature,
                header.eligibilityCertificate.vrfVK,
                header.address
              )
          )
          .leftWiden[BlockHeaderValidationFailure]
      } yield header
  }

  object WithCache {

    /**
     * Wraps an existing BlockHeaderValidation with a cache.  Valid block IDs are saved in the cache to avoid recomputing
     * when switching branches.
     *
     * Invalid block IDs are not saved, but this is subject to change.  This is to avoid an adversary flooding the
     * cache with invalid block IDs, but this comes at the risk of the adversary flooding compute resources.
     *
     * @param underlying The base header validation implementation
     * @param cacheSize The maximum number of header IDs to store
     */
    def make[F[_]: Async](
      underlying: BlockHeaderValidationAlgebra[F],
      cacheSize:  Int = 512
    ): F[BlockHeaderValidationAlgebra[F]] =
      Async[F]
        .delay(CaffeineCache[F, BlockId, Unit](Caffeine.newBuilder.maximumSize(cacheSize).build[BlockId, Entry[Unit]]))
        .map(implicit cache =>
          new BlockHeaderValidationAlgebra[F] {

            def couldBeValidated(header: BlockHeader, currentHead: SlotData): F[Boolean] =
              underlying.couldBeValidated(header, currentHead)

            def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]] =
              cache
                .cachingF(header.id)(ttl = None)(
                  EitherT(Async[F].defer(underlying.validate(header))).void
                    .leftMap(new WrappedFailure(_))
                    .rethrowT
                )
                .as(header.asRight[BlockHeaderValidationFailure])
                .recover { case w: WrappedFailure => w.failure.asLeft[BlockHeader] }

          }
        )

    private class WrappedFailure(val failure: BlockHeaderValidationFailure) extends Exception
  }
}
