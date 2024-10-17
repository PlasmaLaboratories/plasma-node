package org.plasmalabs.codecs.bytes.typeclasses

import com.google.protobuf.ByteString
import scodec.{Attempt, Encoder}

import scala.language.implicitConversions

/**
 * Typeclass for encoding a value into its byte representation for signing routines.
 *
 * @tparam T the value that this typeclass is defined for
 */
trait Signable[T] {

  /**
   * Gets the byte representation of the value that should be used as the message-to-sign.
   * @param value the value to convert into bytes
   * @return an array of bytes representing the signable data of T
   */
  def signableBytes(value: T): ByteString
}

object Signable {

  def apply[A](implicit instance: Signable[A]): Signable[A] = instance

  trait Ops[A] {
    def typeClassInstance: Signable[A]
    def self: A
    def signableBytes: ByteString = typeClassInstance.signableBytes(self)
  }

  trait ToSignableOps {

    implicit def toSignableOps[A](target: A)(implicit tc: Signable[A]): Ops[A] = new Ops[A] {
      val self: A = target
      val typeClassInstance: Signable[A] = tc
    }
  }

  def fromScodecEncoder[T: Encoder]: Signable[T] = t =>
    Encoder[T].encode(t) match {
      case Attempt.Successful(value) => ByteString.copyFrom(value.toByteBuffer)
      case Attempt.Failure(cause)    => throw new IllegalArgumentException(cause.messageWithContext)
    }
}
