package org.plasmalabs.models.protocol

import com.google.protobuf.duration.Duration
import org.plasmalabs.quivr.models.Ratio

case class ConfigGenesis(
  label:                      String,
  fEffective:                 Ratio,
  vrfLddCutoff:               Int,
  vrfPrecision:               Int,
  vrfBaselineDifficulty:      Ratio,
  vrfAmplitude:               Ratio,
  chainSelectionKLookback:    Long,
  slotDuration:               Duration,
  forwardBiasedSlotWindow:    Long,
  operationalPeriodsPerEpoch: Long,
  kesKeyHours:                Int,
  kesKeyMinutes:              Int,
  slotGapLeaderElection:      Long
)
