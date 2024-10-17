package org.plasmalabs.consensus.models

import org.plasmalabs.models.utility.Ratio

case class VrfConfig(lddCutoff: Int, precision: Int, baselineDifficulty: Ratio, amplitude: Ratio)
