package xyz.stratalab.blockchain

import xyz.stratalab.config.ApplicationConfig
import xyz.stratalab.sdk.models.box.Value.ConfigProposal

import scala.concurrent.duration._

object PublicTestnet {

  val DefaultProtocol: ApplicationConfig.Node.Protocol =
    ApplicationConfig.Node.Protocol(
      minAppVersion = "2.0.0",
      fEffective = xyz.stratalab.models.utility.Ratio(12, 100),
      vrfLddCutoff = 15,
      vrfPrecision = 40,
      vrfBaselineDifficulty = xyz.stratalab.models.utility.Ratio(5, 100),
      vrfAmplitude = xyz.stratalab.models.utility.Ratio(50, 100),
      slotGapLeaderElection = 0,
      chainSelectionKLookback = 5184,
      slotDuration = 1.seconds,
      forwardBiasedSlotWindow = 50,
      operationalPeriodsPerEpoch = 25,
      kesKeyHours = 9,
      kesKeyMinutes = 9,
      epochLengthOverride = None
    )

  val DefaultConfigProposal: ConfigProposal =
    BigBang.protocolToConfigProposal(DefaultProtocol)

}
