package org.plasmalabs.consensus.interpreters

import cats.implicits.*
import cats.{Applicative, MonadThrow}
import org.plasmalabs.algebras.*
import org.plasmalabs.algebras.ClockAlgebra.implicits.*
import org.plasmalabs.consensus.algebras.ConsensusValidationStateAlgebra
import org.plasmalabs.consensus.models.{ActiveStaker, BlockId, StakingAddress}
import org.plasmalabs.eventtree.EventSourcedState
import org.plasmalabs.models.*

object ConsensusValidationState {

  /**
   * Constructs a ConsensusValidationState using the combination of an EpochBoundary EventSourcedState and a ConsensusState
   * EventSourcecdState.  Requests for blocks at the tip of the chain will return data from 2 epochs before that block.
   */
  def make[F[_]: MonadThrow](
    genesisBlockId: BlockId,
    epochBoundaryEventSourcedState: EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[
      F
    ], BlockId],
    consensusDataEventSourcedState: EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[
      F
    ], BlockId],
    clock: ClockAlgebra[F]
  ): F[ConsensusValidationStateAlgebra[F]] =
    Applicative[F].pure {
      new ConsensusValidationStateAlgebra[F] {

        def totalActiveStake(currentBlockId: BlockId, slot: Slot): F[BigInt] =
          useStateAtTargetBoundary(currentBlockId, slot)(_.totalActiveStake.getOrRaise(()))

        def staker(currentBlockId: BlockId, slot: Slot)(
          address: StakingAddress
        ): F[Option[ActiveStaker]] =
          useStateAtTargetBoundary(currentBlockId, slot)(_.stakers.get(address))

        /**
         * Determines the N-2 epoch from the given block, then determines the final block ID of the N-2 epoch.  That
         * N-2 block is used in determining the `ConsensusState` to retrieve.  Once retrieved, it is applied to the
         * given `f` function
         */
        private def useStateAtTargetBoundary[Res](
          currentBlockId: BlockId,
          slot:           Slot
        )(f: ConsensusDataEventSourcedState.ConsensusData[F] => F[Res]): F[Res] =
          for {
            epoch <- clock.epochOf(slot)
            // Note: Blocks created within the first two epochs should use the state from the genesis block
            boundaryBlockId <-
              if (epoch > 1)
                epochBoundaryEventSourcedState.useStateAt(currentBlockId)(_.getOrRaise(epoch - 2))
              else
                genesisBlockId.pure[F]
            res <- consensusDataEventSourcedState.useStateAt(boundaryBlockId)(f)
          } yield res
      }
    }
}
