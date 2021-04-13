package co.topl.consensus

import co.topl.attestation.Address
import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.consensus.LeaderElectionProsomo.Config
import co.topl.cryptoprimitives.{Ratio, Vrf}
import co.topl.modifier.block.Block
import co.topl.modifier.box.{ArbitBox, ProgramId}
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.StateReader
import co.topl.utils.{Logging, TimeProvider}
import com.google.common.primitives.Longs
import com.ibm.icu.number.Precision
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.Curve25519

import scala.concurrent.duration.MILLISECONDS
import scala.math.BigInt
import scala.util.Random

object LeaderElectionBifrost extends Logging {

  type TX = Transaction.TX

  type Hit = ArbitBox

  type SR = StateReader[ProgramId, Address]

  /**
    * Gets a hit if it exists on the next block in the chain.
    * @param parent the parent block
    * @param addresses the addresses to stake with
    * @param timestamp the current time
    * @param stateReader a read-only version of state
    * @return a hit if one is found or an error if there are no boxes to stake with
    */
  def getHit(
    parent: Block,
    addresses: Set[Address],
    timestamp: TimeProvider.Time,
    stateReader: SR
  ): Either[NoArbitBoxesError, Option[Hit]] = {
    val arbitBoxes = getArbitBoxes(stateReader, addresses)

    val adjustedTarget: BigDecimal = {
      val target: Double = parent.difficulty.toDouble / consensusStorage.totalStake.toDouble
      val timeDelta = timestamp - parent.timestamp

      BigDecimal(target * timeDelta.toDouble / targetBlockTime(parent.height).toUnit(MILLISECONDS))
    }

    def calculateHit(blockBytes: Array[Byte], boxBytes: Array[Byte]): Long = {
      val hash = Blake2b256(blockBytes ++ boxBytes)

      Longs.fromByteArray((0: Byte) +: hash.take(7))
    }

    // test procedure to determine eligibility
    arbitBoxes
      .map(
        _.map(box => (box, calculateHit(parent.bytes, box.bytes)))
          .filter { test =>
            BigInt(test._2) < (test._1.value.quantity.doubleValue() * adjustedTarget).toBigInt
          }
          .map(h => h._1)
          .headOption)
  }

  def getArbitBoxes(stateReader: SR, addresses: Set[Address]): Either[NoArbitBoxesError, Seq[ArbitBox]] =
    if (addresses.nonEmpty) {
      val boxes = addresses.flatMap {
        stateReader
          .getTokenBoxes(_)
          .getOrElse(Seq())
          .collect { case box: ArbitBox => box }
      }.toSeq

      Right(boxes)
    } else {
      Left(NoArbitBoxesError())
    }

  case class NoArbitBoxesError()
}

object LeaderElectionProsomo extends Logging {

  type Slot = Int
  type Proof = Array[Byte]
  type Hash = Array[Byte]
  type Nonce = Array[Byte]
  type SecretKey = Array[Byte]
  type PublicKey = Array[Byte]

  case class Key(privateKey: SecretKey, publicKey: PublicKey)

  case class Certificate(publicKey: PublicKey, proofHash: Hash, testProof: Proof, threshold: Ratio)

  case class Hit(cert: Certificate, proof: Proof)

  case class Config(lddCutoff: Int, precision: Int, baselineDifficulty: Ratio, amplitude: Ratio)

  /**
    * Gets if the given key is elected for the given slot.
    * @param key the key to stake with
    * @param relativeStake the key's relative stake in the chain
    * @param slot the current slot number
    * @param slotDiff the number of slots since the parent slot
    * @param epochNonce the current epoch's nonce
    * @param config configuration settings
    * @return a hit if the key has been elected for the slot
    */
  def getHit(
    key: Key,
    relativeStake: Ratio,
    slot: Slot,
    slotDiff: Slot, // diff between current slot and parent slot
    epochNonce: Nonce,
    config: Config
  ): Option[Hit] = {
    // private key is 33 bytes, with the first being the type byte (unneeded)
    val privateKeyBytes = key.privateKey.tail

    // create VRF for current state
    val vrf = VrfProof(privateKeyBytes, epochNonce, slot)

    // get the proof used for testing slot eligibility
    val proof = vrf.testProof

    val threshold = getThreshold(relativeStake, slotDiff, config)

    if (isSlotLeaderForThreshold(threshold)(proof))
      Some(Hit(Certificate(key.publicKey, vrf.testProofHashed, proof, threshold), vrf.nonceProof))
    else None
  }

  /** Calculates log(1-f(slot-parentSlot)) or log(1-f) depending on the configuration */
  def mFunction(slotDiff: Int, config: Config): Ratio = {
    // use sawtooth curve if local dynamic difficulty is enabled
    if (slotDiff <= config.lddCutoff)
      ProsomoMath.logOneMinus(
        ProsomoMath.lddGapSawtooth(slotDiff, config.lddCutoff, config.amplitude),
        config.precision)
    else ProsomoMath.logOneMinus(config.baselineDifficulty, config.precision)
  }

  /**
    * Gets the required threshold for the given parameters.
    * @param relativeStake the relative stake in the system
    * @param slotDiff the number of slots between the current slot and the parent slot
    * @param config configuration settings
    * @return the election threshold
    */
  def getThreshold(relativeStake: Ratio, slotDiff: Int, config: Config): Ratio = {
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
  def isSlotLeaderForThreshold(threshold: Ratio)(proof: Proof): Boolean =
    threshold > proof
      .zip(1 to proof.length) // zip with indexes starting from 1
      .foldLeft(Ratio(0)) {
        case (net, (byte, i)) =>
          net + Ratio(BigInt(byte & 0xff), BigInt(2).pow(8 * i))
      }

  case class VrfProof(vrf: Vrf, proofFunc: String => Proof) {
    lazy val testProof: Proof = proofFunc("TEST")
    lazy val nonceProof: Proof = proofFunc("NONCE")
    lazy val testProofHashed: Proof = hash(testProof)

    def hash(input: Array[Byte]): Hash = vrf.Sha512(input)
  }

  object VrfProof {
    def apply(secret: SecretKey, epochNonce: Nonce, slot: Slot): VrfProof = {
      val vrf = new Vrf()
      VrfProof(
        vrf,
        (token: String) =>
          vrf.vrfProof(secret, epochNonce ++ secret ++ BigInt(slot).toByteArray ++ token.getBytes))
    }
  }
}

object ProsomoMath {

  def factorial(n: Int): BigInt = (1 to n).product

  /** Calculates log(1-f) */
  def logOneMinus(f: Ratio, precision: Int): Ratio =
    (1 to precision).foldLeft(Ratio(0))((total, value) => total - (f.pow(value) / value))

  // Local Dynamic Difficulty curve
  def lddGapSawtooth(slotDiff: Int, lddCutoff: Int, amplitude: Ratio): Ratio = Ratio(slotDiff, lddCutoff) * amplitude

}

object LeaderElectionTester extends App {
  val random = new Random(100)
  def randomBytes(l: Int): Array[Byte] = Array.fill(l)((random.nextInt(256) - 128).toByte)

  val key = {
    val k = PrivateKeyCurve25519.secretGenerator.generateSecret(randomBytes(Curve25519.KeyLength))
    LeaderElectionProsomo.Key(k._2.bytes, k._1.bytes)
  }

  val relativeStake = Ratio(1, 10)
  val epochNonce = randomBytes(32)

  // test out hits on slots 1 to 500
  val numberHits = (1 to 500).map(x =>
    LeaderElectionProsomo.getHit(
      key,
      relativeStake,
      x,
      x,
      epochNonce,
      Config(0, 16, Ratio(1, 15), Ratio(2, 5))
    ))
    .count(_.isDefined)

  println(numberHits)
}
