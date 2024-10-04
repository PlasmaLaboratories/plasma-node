package xyz.stratalab.models

import com.google.protobuf.ByteString
import scodec.bits.ByteVector
import xyz.stratalab.node.models.{BlockBody, FullBlockBody}
import xyz.stratalab.sdk.models.TransactionId
import xyz.stratalab.sdk.models.transaction.IoTransaction

import scala.language.implicitConversions

package object utility {

  implicit def byteStringToByteVector(byteString: ByteString): ByteVector =
    ByteVector(byteString.asReadOnlyByteBuffer())

  implicit def byteVectorToByteString(byteVector: ByteVector): ByteString =
    ByteString.copyFrom(byteVector.toByteBuffer)

  implicit def byteStringToByteArray(byteString: ByteString): Array[Byte] = byteString.toByteArray

  implicit def byteArrayToByteString(byteArray: Array[Byte]): ByteString = ByteString.copyFrom(byteArray)

  implicit def scalaDurationToProtoDuration(
    duration: scala.concurrent.duration.FiniteDuration
  ): com.google.protobuf.duration.Duration = {
    import scala.jdk.DurationConverters._
    val j = duration.toJava
    com.google.protobuf.duration.Duration(j.toSeconds, j.toNanosPart)
  }

  implicit def protoDurationToScalaDuration(
    duration: com.google.protobuf.duration.Duration
  ): scala.concurrent.duration.FiniteDuration = {
    import scala.jdk.DurationConverters._
    val j = java.time.Duration.ofSeconds(duration.seconds, duration.nanos)
    j.toScala
  }

  implicit class BlockBodyOps(private val body: BlockBody) extends AnyVal {

    /**
     * Return all Transaction IDs in this block, including the reward transaction ID if provided
     */
    def allTransactionIds: Seq[TransactionId] = body.transactionIds ++ body.rewardTransactionId

  }

  implicit class FullBlockBodyOps(private val body: FullBlockBody) extends AnyVal {

    /**
     * Return all Transactions in this block, including the reward transaction if provided
     * @return
     */
    def allTransactions: Seq[IoTransaction] = body.transactions ++ body.rewardTransaction

  }

}
