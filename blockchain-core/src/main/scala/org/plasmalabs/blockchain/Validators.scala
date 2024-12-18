package org.plasmalabs.blockchain

import cats.effect.implicits.*
import cats.effect.{Async, Resource}
import cats.implicits.*
import org.plasmalabs.algebras.{ClockAlgebra, Stats}
import org.plasmalabs.consensus.algebras.*
import org.plasmalabs.consensus.interpreters.*
import org.plasmalabs.consensus.interpreters.CrossEpochEventSourceState.VotingData
import org.plasmalabs.consensus.models.BlockId
import org.plasmalabs.eventtree.EventSourcedState
import org.plasmalabs.ledger.algebras.*
import org.plasmalabs.ledger.interpreters.*
import org.plasmalabs.ledger.interpreters.ProposalEventSourceState.ProposalEventSourceStateType
import org.plasmalabs.models.{ProposalConfig, VersionId}
import org.plasmalabs.quivr.api.Verifier.instances.verifierInstance
import org.plasmalabs.sdk.validation.algebras.{TransactionAuthorizationVerifier, TransactionSyntaxVerifier}
import org.plasmalabs.sdk.validation.{TransactionAuthorizationInterpreter, TransactionSyntaxInterpreter}
import org.plasmalabs.typeclasses.implicits.*
import org.typelevel.log4cats.Logger

trait Validators[F[_]] {
  def header: BlockHeaderValidationAlgebra[F]
  def headerToBody: BlockHeaderToBodyValidationAlgebra[F]
  def transactionSyntax: TransactionSyntaxVerifier[F]
  def transactionSemantics: TransactionSemanticValidationAlgebra[F]
  def transactionAuthorization: TransactionAuthorizationVerifier[F]
  def bodySyntax: BodySyntaxValidationAlgebra[F]
  def bodySemantics: BodySemanticValidationAlgebra[F]
  def bodyAuthorization: BodyAuthorizationValidationAlgebra[F]
  def boxState: BoxStateAlgebra[F]
  def registrationAccumulator: RegistrationAccumulatorAlgebra[F]
  def rewardCalculator: TransactionRewardCalculatorAlgebra
  def bodyProposalValidationAlgebra: BodyProposalValidationAlgebra[F]
}

case class ValidatorsImpl[F[_]](
  header:                        BlockHeaderValidationAlgebra[F],
  headerToBody:                  BlockHeaderToBodyValidationAlgebra[F],
  transactionSyntax:             TransactionSyntaxVerifier[F],
  transactionSemantics:          TransactionSemanticValidationAlgebra[F],
  transactionAuthorization:      TransactionAuthorizationVerifier[F],
  bodySyntax:                    BodySyntaxValidationAlgebra[F],
  bodySemantics:                 BodySemanticValidationAlgebra[F],
  bodyAuthorization:             BodyAuthorizationValidationAlgebra[F],
  boxState:                      BoxStateAlgebra[F],
  registrationAccumulator:       RegistrationAccumulatorAlgebra[F],
  rewardCalculator:              TransactionRewardCalculatorAlgebra,
  bodyProposalValidationAlgebra: BodyProposalValidationAlgebra[F]
) extends Validators[F]

object Validators {

  // scalastyle:off method.length
  def make[F[_]: Async: Stats: Logger](
    cryptoResources:          CryptoResources[F],
    dataStores:               DataStores[F],
    bigBangBlockId:           BlockId,
    eligibilityCache:         EligibilityCacheAlgebra[F],
    etaCalculation:           EtaCalculationAlgebra[F],
    consensusValidationState: ConsensusValidationStateAlgebra[F],
    leaderElectionThreshold:  LeaderElectionValidationAlgebra[F],
    clockAlgebra:             ClockAlgebra[F],
    boxState:                 BoxStateAlgebra[F],
    registrationAccumulator:  RegistrationAccumulatorAlgebra[F],
    versionsEventSourceState: EventSourcedState[F, VotingData[F], BlockId],
    proposalEventState:       ProposalEventSourceStateType[F],
    config:                   ProposalConfig,
    maxSupportedVersion:      VersionId
  ): Resource[F, Validators[F]] =
    for {
      blockHeaderVersionValidation <- BlockHeaderVersionValidation
        .make[F](
          clockAlgebra,
          versionsEventSourceState,
          maxSupportedVersion
        )
        .toResource
      blockHeaderVotingValidation <- BlockHeaderVotingValidation
        .make[F](clockAlgebra, versionsEventSourceState)
        .toResource
      headerValidation <- BlockHeaderValidation
        .make[F](
          etaCalculation,
          consensusValidationState,
          leaderElectionThreshold,
          blockHeaderVersionValidation,
          blockHeaderVotingValidation,
          eligibilityCache,
          clockAlgebra,
          dataStores.headers,
          bigBangBlockId,
          cryptoResources.ed25519VRF,
          cryptoResources.kesProduct,
          cryptoResources.ed25519,
          cryptoResources.blake2b256
        )
        .flatMap(BlockHeaderValidation.WithCache.make[F](_))
        .toResource
      headerToBody <- BlockHeaderToBodyValidation.make().toResource
      transactionSyntaxValidation = TransactionSyntaxInterpreter.make[F]()
      transactionSemanticValidation <- TransactionSemanticValidation
        .make[F](dataStores.transactions.get, boxState)
      transactionAuthorizationValidation = TransactionAuthorizationInterpreter.make[F]()
      rewardCalculator <- TransactionRewardCalculator.make[F]
      bodySyntaxValidation <- BodySyntaxValidation
        .make[F](dataStores.transactions.getOrRaise, transactionSyntaxValidation, rewardCalculator)
        .toResource
      bodySemanticValidation <- BodySemanticValidation
        .make[F](
          dataStores.transactions.getOrRaise,
          transactionSemanticValidation,
          registrationAccumulator
        )
        .toResource
      bodyAuthorizationValidation <- BodyAuthorizationValidation
        .make[F](
          dataStores.transactions.getOrRaise,
          transactionAuthorizationValidation
        )
        .toResource
      bodyProposalValidation <- BodyProposalValidation
        .make[F](
          clockAlgebra,
          dataStores.transactions.getOrRaise,
          proposalEventState,
          config
        )
        .toResource

    } yield ValidatorsImpl(
      headerValidation,
      headerToBody,
      transactionSyntaxValidation,
      transactionSemanticValidation,
      transactionAuthorizationValidation,
      bodySyntaxValidation,
      bodySemanticValidation,
      bodyAuthorizationValidation,
      boxState,
      registrationAccumulator,
      rewardCalculator,
      bodyProposalValidation
    )

  // scalastyle:on method.length
}
