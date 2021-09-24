package co.topl.minting

import cats.Monad
import cats.implicits._
import co.topl.consensus.LeaderElectionValidation
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.consensus.vrf.ProofToHash
import co.topl.crypto.signatures.Ed25519VRF
import co.topl.crypto.typeclasses.implicits._
import co.topl.minting.algebras.LeaderElectionMintingAlgebra
import co.topl.minting.algebras.LeaderElectionMintingAlgebra.VrfHit
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Ratio, Sized}

object LeaderElectionMinting {

  object Eval {

    def make[F[_]: Monad](
      secret:               SecretKeys.Vrf,
      thresholdInterpreter: LeaderElectionValidationAlgebra[F]
    ): LeaderElectionMintingAlgebra[F] =
      (relativeStake: Ratio, slot: Slot, slotDiff: Epoch, eta: Eta) =>
        thresholdInterpreter
          .getThreshold(relativeStake, slotDiff)
          .flatMap { threshold =>
            val testProof =
              VrfProof(secret, LeaderElectionValidation.VrfArgument(eta, slot, LeaderElectionValidation.Tokens.Test))
            thresholdInterpreter
              .isSlotLeaderForThreshold(threshold)(ProofToHash.digest(testProof))
              .map(isSlotLeader =>
                if (isSlotLeader)
                  VrfHit(
                    EligibilityCertificate(
                      VrfProof(
                        secret,
                        LeaderElectionValidation.VrfArgument(eta, slot, LeaderElectionValidation.Tokens.Nonce)
                      ),
                      testProof,
                      secret.verificationKey[VerificationKeys.Vrf],
                      ???
                    ),
                    slot,
                    threshold
                  ).some
                else
                  none[VrfHit]
              )
          }

    object VrfProof {

      def apply(skVrf: SecretKeys.Vrf, arg: LeaderElectionValidation.VrfArgument): Proofs.Signature.VrfEd25519 =
        Proofs.Signature.VrfEd25519(
          Sized.strictUnsafe(
            Bytes(
              Ed25519VRF.instance.vrfProof(
                skVrf.ed25519.bytes.data.toArray,
                arg.signableBytes.toArray
              )
            )
          )
        )
    }
  }
}
