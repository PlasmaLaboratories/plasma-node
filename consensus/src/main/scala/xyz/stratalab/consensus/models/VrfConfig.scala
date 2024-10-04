package xyz.stratalab.consensus.models

import xyz.stratalab.models.utility.Ratio

case class VrfConfig(lddCutoff: Int, precision: Int, baselineDifficulty: Ratio, amplitude: Ratio)
