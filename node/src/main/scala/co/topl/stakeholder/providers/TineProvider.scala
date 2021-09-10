package co.topl.stakeholder.providers

import akka.actor.{Actor, PoisonPill, Props, Timers}
import akka.util.Timeout
import co.topl.stakeholder.cases._
import co.topl.stakeholder.components.{Block, Serializer}
import co.topl.stakeholder.history.BlockStorage
import co.topl.stakeholder.primitives._
import co.topl.settings.AppSettings

import scala.concurrent.Await
import scala.util.control.Breaks.{break, breakable}

class TineProvider(blockStorage: BlockStorage, localRef: ActorRefWrapper, settings: AppSettings)(implicit
  routerRef:                     ActorRefWrapper
) extends Actor
    with Timers
    with Types {
  val sig: Sig = new Sig
  val serializer: Serializer = new Serializer
  override val fch = new Fch
  val waitTime = TetraParameters.waitTime
  val requestTineInterval = TetraParameters.requestTineInterval

  def send(sender: ActorRefWrapper, ref: ActorRefWrapper, command: Any): Unit = {
    implicit val timeout: Timeout = Timeout(waitTime)
    val future = if (ref.remote) {
      routerRef ? TineProvider.Egress(Left(MessageFromLocalToRemote(sender, ref.path, command)))
    } else {
      localRef ? TineProvider.Egress(Right(MessageFromLocalToLocal(sender, ref, command)))
    }
    val result = Await.result(future, timeout.duration)
    assert(result == "done")
  }

  override def receive: Receive = {
    case TineProvider.Info(
          holderIndex: Int,
          ref: ActorRefWrapper,
          holderRef: ActorRefWrapper,
          startId: SlotId,
          depth: Int,
          job: Int,
          nextBlocks: Option[Array[SlotId]],
          inbox: Option[Map[Sid, (ActorRefWrapper, PublicKeys)]]
        ) =>
      var returnedIdList: List[SlotId] = List()
      var id: SlotId = startId
      if (job == -1) {
        // job -1 means fetch info from hello message
        breakable {
          for (id <- nextBlocks.get) {
            if (returnedIdList.length < depth) {
              blockStorage.restoreBlock(id) match {
                case Some(block: Block) =>
                  returnedIdList ::= id
                  send(
                    holderRef,
                    ref,
                    ReturnBlocks(List(block), -1, holderRef)
                  )
                case None => break()
              }
            } else {
              break()
            }
            Thread.sleep(requestTineInterval)
          }
        }
        //sends new bootstrappers inbox info so they have the diffused messages when they come to the head
        inbox match {
          case Some(data) =>
            for (entry <- data)
              send(
                holderRef,
                ref,
                DiffuseData(entry._1, entry._2._1, entry._2._2, holderRef)
              )
          case None =>
        }
      } else {
        breakable {
          while (returnedIdList.length < depth) {
            blockStorage.restoreBlock(id) match {
              case Some(block: Block) =>
                returnedIdList ::= id
                send(
                  holderRef,
                  ref,
                  ReturnBlocks(List(block), job, holderRef)
                )
                id = block.parentSlotId
              case None => break()
            }
            Thread.sleep(requestTineInterval)
          }
        }
      }
      self ! PoisonPill
  }

  override def postStop(): Unit =
    context.parent ! TineProvider.Done
}

object TineProvider extends SimpleTypes {

  case class Info(
    holderIndex: Int,
    ref:         ActorRefWrapper,
    holderRef:   ActorRefWrapper,
    startId:     SlotId,
    depth:       Int,
    job:         Int,
    nextBlocks:  Option[Array[SlotId]],
    inbox:       Option[Map[Sid, (ActorRefWrapper, PublicKeys)]]
  )

  case class Egress(content: Either[MessageFromLocalToRemote, MessageFromLocalToLocal])

  case object Done

  def props(blockStorage: BlockStorage, localRef: ActorRefWrapper, settings: AppSettings)(implicit
    routerRef:            ActorRefWrapper
  ): Props =
    Props(new TineProvider(blockStorage, localRef, settings: AppSettings))
      .withDispatcher(TetraParameters.tineProviderEC)
}
