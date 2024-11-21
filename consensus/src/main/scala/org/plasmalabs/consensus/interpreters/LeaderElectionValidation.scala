package org.plasmalabs.consensus.interpreters

import cats.effect.*
import cats.implicits.*
import org.plasmalabs.consensus.algebras.LeaderElectionValidationAlgebra
import org.plasmalabs.consensus.models.VrfConfig
import org.plasmalabs.consensus.rhoToRhoTestHash
import org.plasmalabs.crypto.hash.Blake2b512
import org.plasmalabs.models.*
import org.plasmalabs.models.utility.Ratio
import org.plasmalabs.numerics.algebras.{Exp, Log1p}
import org.plasmalabs.numerics.implicits.*
import scalacache.caffeine.CaffeineCache

/**
 * Credit to Aaron Schutza
 */
object LeaderElectionValidation {

  /**
   * Normalization constant for test nonce hash evaluation based on 512 byte hash function output
   */
  private val NormalizationConstant: BigInt = BigInt(2).pow(512)

  def make[F[_]: Sync](
    config:                VrfConfig,
    slotGapLeaderElection: Long,
    blake2b512Resource:    Resource[F, Blake2b512],
    exp:                   Exp[F],
    log1p:                 Log1p[F]
  ): LeaderElectionValidationAlgebra[F] =
    new LeaderElectionValidationAlgebra[F] {

      def getThreshold(relativeStake: Ratio, slotDiff: Long): F[Ratio] = {
        val difficultyCurve: Ratio =
          if (slotDiff <= slotGapLeaderElection) Ratio.Zero
          else if (slotDiff > config.lddCutoff) config.baselineDifficulty
          else {
            val numerator = BigInt(slotDiff - slotGapLeaderElection)
            val denominator = BigInt(config.lddCutoff - slotGapLeaderElection)
            Ratio(numerator, denominator) * config.amplitude
          }

        difficultyCurve match {
          case Ratio.One  => Ratio.One.pure[F]
          case Ratio.Zero => Ratio.Zero.pure[F]
          case _ =>
            for {
              coefficient <- log1p.evaluate(Ratio.NegativeOne * difficultyCurve)
              result      <- exp.evaluate(coefficient * relativeStake)
            } yield Ratio.One - result
        }
      }

      /**
       * Determines if the given proof meets the threshold to be elected slot leader
       * @param threshold the threshold to reach
       * @param rho the randomness
       * @return true if elected slot leader and false otherwise
       */
      def isSlotLeaderForThreshold(threshold: Ratio)(rho: Rho): F[Boolean] =
        blake2b512Resource
          .use(implicit blake2b512 => Sync[F].delay(rhoToRhoTestHash(rho.sizedBytes.data).toByteArray))
          .map { testRhoHashBytes =>
            val test = Ratio(BigInt(Array(0x00.toByte) ++ testRhoHashBytes), NormalizationConstant, BigInt(1))
            (threshold > test)
          }
    }

  def makeCached[F[_]: Sync](alg: LeaderElectionValidationAlgebra[F]): F[LeaderElectionValidationAlgebra[F]] =
    CaffeineCache[F, (Ratio, Slot), Ratio].map(cache =>
      new LeaderElectionValidationAlgebra[F] {

        override def getThreshold(relativeStake: Ratio, slotDiff: Long): F[Ratio] =
          cache.cachingF((relativeStake, slotDiff))(ttl = None)(
            Sync[F].defer(
              alg.getThreshold(relativeStake, slotDiff)
            )
          )

        override def isSlotLeaderForThreshold(threshold: Ratio)(rho: Rho): F[Boolean] =
          alg.isSlotLeaderForThreshold(threshold)(rho)
      }
    )

  /**
   * The threshold calculation should return a constant value for any slotDiff exceeding the lddCutoff
   * @param alg The underlying interpreter
   * @param lddCutoff The VRF's lddCutoff parameter
   * @return a re-interpreted LeaderElectionValidationAlgebra
   */
  def makeWithCappedSlotDiff[F[_]](
    alg:       LeaderElectionValidationAlgebra[F],
    lddCutoff: Long
  ): LeaderElectionValidationAlgebra[F] =
    new LeaderElectionValidationAlgebra[F] {

      /**
       * If slotDiff > lddCutoff, substitute (lddCutoff + 1) for the slotDiff since the result should
       * always be constant
       */
      def getThreshold(relativeStake: Ratio, slotDiff: Long): F[Ratio] =
        if (slotDiff > lddCutoff) alg.getThreshold(relativeStake, lddCutoff + 1)
        else alg.getThreshold(relativeStake, slotDiff)

      def isSlotLeaderForThreshold(threshold: Ratio)(rho: Rho): F[Boolean] =
        alg.isSlotLeaderForThreshold(threshold)(rho)
    }

}
