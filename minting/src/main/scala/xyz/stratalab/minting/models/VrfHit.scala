package org.plasmalabs.minting.models

import org.plasmalabs.consensus.models.EligibilityCertificate
import org.plasmalabs.models.Slot
import org.plasmalabs.models.utility.Ratio

case class VrfHit(cert: EligibilityCertificate, slot: Slot, threshold: Ratio)
