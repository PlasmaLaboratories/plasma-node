package co.topl.blockchain

import cats.effect.{Async, Resource}
import cats.effect.implicits._
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.brambl.validation.TransactionAuthorizationInterpreter
import co.topl.brambl.validation.TransactionSyntaxInterpreter
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.consensus.algebras._
import co.topl.consensus.interpreters._
import co.topl.consensus.models.BlockId
import co.topl.ledger.algebras._
import co.topl.ledger.interpreters._
import co.topl.quivr.api.Verifier.instances.verifierInstance
import co.topl.typeclasses.implicits._
import co.topl.brambl.validation.algebras.TransactionAuthorizationVerifier
import co.topl.algebras.Stats
import co.topl.consensus.interpreters.VotingEventSourceState.VotingData
import co.topl.eventtree.EventSourcedState
import co.topl.ledger.interpreters.ProposalEventSourceState.ProposalEventSourceStateType
import co.topl.models.ProposalConfig
import co.topl.models.VersionId
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
        .make[F](dataStores.transactions.getOrRaise, boxState)
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
