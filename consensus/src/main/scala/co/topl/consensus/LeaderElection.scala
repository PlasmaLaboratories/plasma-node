package co.topl.consensus

import co.topl.consensus.crypto.Vrf
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.Lengths._
import co.topl.models._
import co.topl.models.utility.{Lengths, Ratio, Sized}
import co.topl.typeclasses.RatioOps.implicits._

object LeaderElection {

  sealed abstract class Failure

  object Failures {
    case class ThresholdNotMet(threshold: Ratio, proof: Bytes) extends Failure
  }

  case class Hit(cert: VrfCertificate, proof: Bytes, slot: Slot)

  case class Config(lddCutoff: Int, precision: Int, baselineDifficulty: Ratio, amplitude: Ratio)

  def hits(
    secret:        Secrets.Vrf,
    relativeStake: Ratio,
    fromSlot:      Slot,
    epochNonce:    Nonce,
    config:        Config
  ): LazyList[Hit] =
    LazyList
      .unfold(fromSlot)(s => Some(s + 1, s + 1))
      .map(slot => getHit(secret, relativeStake, slot, slot - fromSlot, epochNonce, config))
      .collect { case Right(hit) => hit }

  /**
   * Gets if the given key is elected for the given slot.
   * @param secret the key to stake with
   * @param relativeStake the key's relative stake in the chain
   * @param slot the current slot number
   * @param slotDiff the number of slots since the parent slot
   * @param epochNonce the current epoch's nonce
   * @param config configuration settings
   * @return a hit if the key has been elected for the slot
   */
  def getHit(
    secret:        Secrets.Vrf,
    relativeStake: Ratio,
    slot:          Slot,
    slotDiff:      Long, // diff between current slot and parent slot
    epochNonce:    Nonce,
    config:        Config
  ): Either[Failure, Hit] = {
    // private key is 33 bytes, with the first being the type byte (unneeded)
    val privateKeyBytes = secret.privateKey.ed25519.bytes.data

    // create VRF for current state
    val vrf = VrfProof(privateKeyBytes, epochNonce, slot)

    // get the proof used for testing slot eligibility
    val proof = vrf.testProof

    val threshold = getThreshold(relativeStake, slotDiff, config)

    Either.cond(
      isSlotLeaderForThreshold(threshold)(proof),
      Hit(
        VrfCertificate(
          secret.publicKey,
          Sized.strict[Bytes, Lengths.`64`.type](vrf.testProofHashed).toOption.get,
          Sized.strict[Bytes, Lengths.`80`.type](proof).toOption.get
        ),
        vrf.nonceProof,
        slot
      ),
      Failures.ThresholdNotMet(threshold, proof)
    )
  }

  /** Calculates log(1-f(slot-parentSlot)) or log(1-f) depending on the configuration */
  def mFunction(slotDiff: Long, config: Config): Ratio =
    // use sawtooth curve if local dynamic difficulty is enabled
    if (slotDiff <= config.lddCutoff)
      ProsomoMath.logOneMinus(
        ProsomoMath.lddGapSawtooth(slotDiff, config.lddCutoff, config.amplitude),
        config.precision
      )
    else ProsomoMath.logOneMinus(config.baselineDifficulty, config.precision)

  /**
   * Gets the required threshold for the given parameters.
   * @param relativeStake the relative stake in the system
   * @param slotDiff the number of slots between the current slot and the parent slot
   * @param config configuration settings
   * @return the election threshold
   */
  def getThreshold(relativeStake: Ratio, slotDiff: Long, config: Config): Ratio = {
    val mFValue = mFunction(slotDiff, config)
    val base = mFValue * relativeStake

    (1 to config.precision)
      .foldLeft(Ratio(0))((total, i) => total - (base.pow(i) * Ratio(BigInt(1), ProsomoMath.factorial(i))))
  }

  /**
   * Determines if the given proof meets the threshold to be elected slot leader
   * @param threshold the threshold to reach
   * @param proof the proof output
   * @return true if elected slot leader and false otherwise
   */
  def isSlotLeaderForThreshold(threshold: Ratio)(proof: Bytes): Boolean =
    threshold > proof
      .zip(1 to proof.length) // zip with indexes starting from 1
      .foldLeft(Ratio(0)) { case (net, (byte, i)) =>
        net + Ratio(BigInt(byte & 0xff), BigInt(2).pow(8 * i))
      }

  case class VrfProof(vrf: Vrf, proofFunc: String => Bytes) {
    lazy val testProof: Bytes = proofFunc("TEST")
    lazy val nonceProof: Bytes = proofFunc("NONCE")
    lazy val testProofHashed: Hash = hash(testProof.unsafeArray.asInstanceOf[Array[Byte]])

    def hash(input: Array[Byte]): Hash = Bytes(vrf.Sha512(input))
  }

  object VrfProof {

    def apply(secretData: Bytes, epochNonce: Nonce, slot: Slot): VrfProof = {
      val vrf = new Vrf()
      VrfProof(
        vrf,
        (token: String) =>
          Bytes(
            vrf.vrfProof(
              secretData.unsafeArray.asInstanceOf[Array[Byte]],
              (epochNonce ++ secretData ++ BigInt(slot).toByteArray ++ token.getBytes).toArray[Byte]
            )
          )
      )
    }
  }
}

object ProsomoMath {

  def factorial(n: Int): BigInt = (1 to n).product

  /** Calculates log(1-f) */
  def logOneMinus(f: Ratio, precision: Int): Ratio =
    (1 to precision).foldLeft(Ratio(0))((total, value) => total - (f.pow(value) / value))

  // Local Dynamic Difficulty curve
  def lddGapSawtooth(slotDiff: Long, lddCutoff: Int, amplitude: Ratio): Ratio = Ratio(slotDiff, lddCutoff) * amplitude

}
