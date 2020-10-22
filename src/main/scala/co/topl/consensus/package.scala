package co.topl

import co.topl.modifier.block.Block
import co.topl.nodeView.state.box.ArbitBox
import com.google.common.primitives.Longs
import scorex.crypto.hash.Blake2b256

import scala.concurrent.duration._
import scala.math.{ max, min }

package object consensus {
  // these variables are left as vars since they MUST be determined at runtime from the config file
  // todo: JAA - figure out a better way to do this
  private var _maxStake: Long = _
  private var _inflation: Long = _
  private var _targetBlockTime: FiniteDuration = _
  private var _difficulty: Long = _
  private var _numTxInBlock: Int = _

  // setters
  private[consensus] def maxStake_= (value: Long): Unit = _maxStake = value
  private[consensus] def inflation_= (value: Long): Unit = _inflation = value
  private[consensus] def targetBlockTime_= (value: FiniteDuration): Unit = _targetBlockTime = value
  private[consensus] def difficulty_= (value: Long): Unit = _difficulty = value
  private[consensus] def numTxInBlock_= (value: Int): Unit = _numTxInBlock = value

  // getters
  def maxStake: Long = _maxStake
  def inflation: Long = _inflation
  def targetBlockTime: FiniteDuration = _targetBlockTime
  def difficulty: Long = _difficulty
  def numTxInBlock: Int = _numTxInBlock

  /**
   * Defines how we calculate the test value for determining eligibility to forge
   *
   * @param lastBlock previous block
   * @param box       box to be used for the test value
   * @return the test value to be compared to the adjusted difficulty
   */
  def calcHit(lastBlock: Block)(box: ArbitBox): Long = {
    val h = Blake2b256(lastBlock.bytes ++ box.bytes)

    Longs.fromByteArray((0: Byte) +: h.take(7))
  }

  /**
   * Calculates the adjusted difficulty for forging based on the time passed since the previous block
   *
   * @param parent         previous block
   * @param baseDifficulty base difficulty of the parent block
   * @param timestamp      the current timestamp
   * @return the adjusted difficulty
   */
  def calcAdjustedTarget(parent: Block,
                         baseDifficulty: Long,
                         timestamp: Long): BigDecimal = {

    val target: Double = baseDifficulty.toDouble / maxStake.toDouble
    val timeDelta = timestamp - parent.timestamp

    BigDecimal(target * timeDelta.toDouble / targetBlockTime.toUnit(MILLISECONDS))
  }

  /**
    * Calculate the block difficulty according to
    * [[https://nxtdocs.jelurida.com/Nxt_Whitepaper#Block_Creation_.28Forging.29]]
    *
    * @param prevDifficulty the previous base difficulty
    * @param prevTimes      sequence of block times to calculate the average and compare to target
    * @return the modified difficulty
    */
  def calcNewBaseDifficulty(prevDifficulty: Long, prevTimes: Seq[Block.Timestamp]): Long = {
    val averageDelay = (prevTimes drop 1, prevTimes).zipped.map(_-_).sum / (prevTimes.length - 1)
    val targetTimeMilli = targetBlockTime.toUnit(MILLISECONDS)

    // magic numbers here (1.1, 0.9, and 0.64) are straight from NXT
    if (averageDelay > targetTimeMilli) {
      (prevDifficulty * min(averageDelay, targetTimeMilli * 1.1) / targetTimeMilli).toLong
    } else {
      (prevDifficulty * (1 - 0.64 * (1 - (max(averageDelay, targetTimeMilli * 0.9) / targetTimeMilli) ))).toLong
    }
  }
}
