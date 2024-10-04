package xyz.stratalab.minting.models

import co.topl.consensus.models.EligibilityCertificate
import xyz.stratalab.models.Slot
import xyz.stratalab.models.utility.Ratio

case class VrfHit(cert: EligibilityCertificate, slot: Slot, threshold: Ratio)
