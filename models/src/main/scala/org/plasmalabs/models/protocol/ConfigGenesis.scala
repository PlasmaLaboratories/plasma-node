package org.plasmalabs.models.protocol

case class ConfigGenesis(
  label:                      String,
  fEffective:                 quivr.models.Ratio,
  vrfLddCutoff:               Int,
  vrfPrecision:               Int,
  vrfBaselineDifficulty:      quivr.models.Ratio,
  vrfAmplitude:               quivr.models.Ratio,
  chainSelectionKLookback:    Long,
  slotDuration:               com.google.protobuf.duration.Duration,
  forwardBiasedSlotWindow:    Long,
  operationalPeriodsPerEpoch: Long,
  kesKeyHours:                Int,
  kesKeyMinutes:              Int,
  slotGapLeaderElection:      Long
)
