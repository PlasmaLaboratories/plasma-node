package examples.bifrost.forging

import akka.actor.{Actor, ActorRef}
import com.google.common.primitives.Longs
import examples.bifrost.blocks.BifrostBlock
import examples.bifrost.transaction.box.StableCoinBox
//TODO fix this to BifrostHistory
import examples.curvepos.SimpleBlockchain
import examples.curvepos.transaction._
import scorex.core.LocalInterface.LocallyGeneratedModifier
import examples.bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.utils.{NetworkTime, ScorexLogging}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Try

trait ForgerSettings extends Settings {
}

class Forger(forgerSettings: ForgingSettings, viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  import Forger._

  //should be a part of consensus, but for our app is okay
  val TransactionsInBlock = 100

  //set to true for initial generator
  private var forging = forgerSettings.offlineGeneration

  private val hash = FastCryptographicHash


  val InterBlocksDelay = 15 //in seconds
  val blockGenerationDelay = 5000.millisecond

  override def preStart(): Unit = {
    if (forging) context.system.scheduler.scheduleOnce(1.second)(self ! Forge)
  }

  private def bounded(value: BigInt, min: BigInt, max: BigInt): BigInt =
    if (value < min) min else if (value > max) max else value

  private def calcBaseTarget(lastBlock: SimpleBlock,
                             currentTime: Long): Long = {
    val eta = currentTime - lastBlock.timestamp
    val prevBt = BigInt(lastBlock.baseTarget)
    val t0 = bounded(prevBt * eta / InterBlocksDelay, prevBt / 2, prevBt * 2)
    bounded(t0, 1, Long.MaxValue).toLong
  }

  protected def calcTarget(lastBlock: SimpleBlock,
                           state: SimpleState,
                           generator: PublicKey25519Proposition): BigInt = {
    val eta = (NetworkTime.time() - lastBlock.timestamp) / 1000 //in seconds
    val balance = state.boxesOf(generator).headOption.map(_.value).getOrElse(0L)
    BigInt(lastBlock.baseTarget) * eta * balance
  }

  private def calcGeneratorSignature(lastBlock: SimpleBlock, generator: PublicKey25519Proposition) =
    hash(lastBlock.generationSignature ++ generator.pubKeyBytes)

  private def calcHit(lastBlock: SimpleBlock, generator: PublicKey25519Proposition): BigInt =
    BigInt(1, calcGeneratorSignature(lastBlock, generator).take(8))

  override def receive: Receive = {
    case StartMining =>
      forging = true
      context.system.scheduler.scheduleOnce(blockGenerationDelay)(self ! Forge)

    case StopMining =>
      forging = false

    case CurrentView(history: SimpleBlockchain, state: SimpleState, wallet: SimpleWallet, memPool: SimpleMemPool) =>
      log.info("Trying to generate a new block, chain length: " + history.height())

      val lastBlock = history.lastBlock
      val generators: Set[PublicKey25519Proposition] = wallet.publicKeys
      lazy val toInclude = state.filterValid(memPool.take(TransactionsInBlock).toSeq)

      val generatedBlocks = generators.flatMap { generator =>
        val hit = calcHit(lastBlock, generator)
        val target = calcTarget(lastBlock, state, generator)
        if (hit < target) {
          Try {
            val timestamp = NetworkTime.time()
            val bt = calcBaseTarget(lastBlock, timestamp)
            val secret: PrivateKey25519 = wallet.secretByPublicImage(generator).get

            val unsigned: SimpleBlock = SimpleBlock(lastBlock.id, timestamp, Array(), bt, generator, toInclude)
            val signature = PrivateKey25519Companion.sign(secret, unsigned.serializer.messageToSign(unsigned))
            val signedBlock = unsigned.copy(generationSignature = signature.signature)
            log.info(s"Generated new block: ${signedBlock.json.noSpaces}")
            LocallyGeneratedModifier[PublicKey25519Proposition, SimpleTransaction, SimpleBlock](signedBlock)
          }.toOption
        } else {
          None
        }
      }
      generatedBlocks.foreach(localModifier => viewHolderRef ! localModifier)
      context.system.scheduler.scheduleOnce(blockGenerationDelay)(self ! Forge)

    case Forge =>
      viewHolderRef ! GetCurrentView
  }
}

object Forger {

  val InitialDifficuly = 15000000000L
  val MaxTarget = Long.MaxValue

  case object StartMining

  case object StopMining

  case object Forge

  def hit(lastBlock: BifrostBlock)(box: StableCoinBox): Long = {
    val h = FastCryptographicHash(lastBlock.bytes ++ box.bytes)
    Longs.fromByteArray((0: Byte) +: h.take(7))
  }
}