package scorex.core.network.message

import java.nio.ByteOrder

import akka.actor.DeadLetterSuppression
import akka.util.ByteString
import scorex.core.network.{ConnectedPeer, MaliciousBehaviorException}
import scorex.crypto.hash.Blake2b256

import scala.util.{Success, Try}

case class Message[Content](spec: MessageSpec[Content],
                            input: Either[Array[Byte], Content],
                            source: Option[ConnectedPeer])
  extends DeadLetterSuppression {

  import Message._

  lazy val dataBytes: Array[Byte] = input match {
    case Left(db) => db
    case Right(d) => spec.toBytes(d)
  }

  lazy val data: Try[Content] = input match {
    case Left(db) => spec.parseBytesTry(db)
    case Right(d) => Success(d)
  }

  lazy val dataLength: Int = dataBytes.length

  def messageLength: Int = {
    if (dataLength > 0) HeaderLength + ChecksumLength + dataLength else HeaderLength
  }
}

class MessageSerializer(specs: Seq[MessageSpec[_]], magicBytes: Array[Byte]) {

  import Message.{ChecksumLength, HeaderLength, MagicLength}

  import scala.language.existentials

  private implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN

  private val specsMap = Map(specs.map(s => s.messageCode -> s): _*)
    .ensuring(m => m.size == specs.size, "Duplicate message codes")

  def serialize(obj: Message[_]): ByteString = {
    val builder = ByteString.createBuilder
      .putBytes(magicBytes)
      .putByte(obj.spec.messageCode)
      .putInt(obj.dataLength)

    if (obj.dataLength > 0) {
      val checksum = Blake2b256.hash(obj.dataBytes).take(ChecksumLength)
      builder.putBytes(checksum).putBytes(obj.dataBytes)
    }

    builder.result()
  }

  //MAGIC ++ Array(spec.messageCode) ++ Ints.toByteArray(dataLength) ++ dataWithChecksum
  def deserialize(byteString: ByteString, sourceOpt: Option[ConnectedPeer]): Try[Option[Message[_]]] = Try {
    if (byteString.length < HeaderLength) {
      None
    } else {
      val it = byteString.iterator
      val magic = it.getBytes(MagicLength)
      val msgCode = it.getByte
      val length = it.getInt

      //peer is from another network
      if (!java.util.Arrays.equals(magic, magicBytes)) {
        throw MaliciousBehaviorException(s"Wrong magic bytes, expected ${magicBytes.mkString}, got ${magic.mkString}")
      }

      //peer is trying to cause buffer overflow or breaking the parsing
      if (length < 0) {
        throw MaliciousBehaviorException("Data length is negative!")
      }

      val spec = specsMap.getOrElse(msgCode, throw new Error(s"No message handler found for $msgCode"))

      if (length != 0 && byteString.length < length + HeaderLength + ChecksumLength) {
        None
      } else {
        val msgData = if (length > 0) {
          val checksum = it.getBytes(ChecksumLength)
          val data = it.getBytes(length)
          val digest = Blake2b256.hash(data).take(ChecksumLength)

          //peer reported incorrect checksum
          if (!java.util.Arrays.equals(checksum, digest)) {
            throw MaliciousBehaviorException(s"Wrong checksum, expected ${checksum.mkString}, got ${checksum.mkString}")
          }
          data
        } else {
          Array.empty[Byte]
        }

        Some(Message(spec, Left(msgData), sourceOpt))
      }
    }
  }
}

object Message {
  type MessageCode = Byte

  val MagicLength: Int = 4
  val ChecksumLength: Int = 4
  val HeaderLength: Int = MagicLength + 5
}
