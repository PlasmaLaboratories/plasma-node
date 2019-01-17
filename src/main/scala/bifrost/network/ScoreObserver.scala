package bifrost.network

import akka.actor.{Actor, ActorRef}
import bifrost.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

//todo: break a connection if no score message from remote for some time?

class ScoreObserver(historySynchronizer: ActorRef) extends Actor with ScorexLogging {

  import ScoreObserver._

  private case class Candidate(peer: ConnectedPeer, score: BigInt, seen: Long)

  private var candidates = Seq[Candidate]()

  private def consider(candidates: Seq[Candidate]): (Option[Candidate], Seq[Candidate]) =
    candidates.isEmpty match {
      case true => (None, Seq())
      case false =>
        val bestNetworkScore = candidates.maxBy(_.score).score
        val witnesses = candidates.filter(_.score == bestNetworkScore)
        (witnesses.headOption, witnesses)
    }

  private def clearOld(candidates: Seq[Candidate]): Seq[Candidate] = {
    //todo: make configurable?
    val threshold = System.currentTimeMillis() - 1.minute.toMillis
    candidates.filter(_.seen > threshold)
  }

  override def preStart: Unit = {
    //todo: make configurable?
    context.system.scheduler.schedule(5.seconds, 5.seconds)(self ! UpdateScore(None))
  }

  override def receive: Receive = {
    case UpdateScore(scoreToAddOpt) =>
      val oldScore = candidates.headOption.map(_.score)
      candidates = clearOld(candidates)

      scoreToAddOpt.foreach { case (connectedPeer, value) =>
        candidates = candidates.filter(_.peer != connectedPeer)
        candidates = candidates :+ Candidate(connectedPeer, value, System.currentTimeMillis())
      }

      val ct = consider(candidates)
      candidates = ct._2

      val newScore = ct._1.map(_.score)
      val witnesses = candidates.map(_.peer)

      if (newScore.getOrElse(BigInt(0)) != oldScore.getOrElse(BigInt(0))) {
        historySynchronizer ! ConsideredValue(newScore, witnesses)
      }

    case GetScore =>
      candidates = clearOld(candidates)
      candidates.headOption.map(_.score) match {
        case None => context.system.scheduler.scheduleOnce(1.second, sender(), ConsideredValue(None, Seq()))
        case score => sender() ! ConsideredValue(score, candidates.map(_.peer))
      }
  }
}

object ScoreObserver {
  case object GetScore
  case class UpdateScore(scoreToAdd: Option[(ConnectedPeer, BigInt)])
  case class ConsideredValue(value: Option[BigInt], witnesses: Seq[ConnectedPeer])
}
