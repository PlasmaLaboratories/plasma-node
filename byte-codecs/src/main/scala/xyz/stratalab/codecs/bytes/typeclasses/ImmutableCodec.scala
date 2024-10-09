package xyz.stratalab.codecs.bytes.typeclasses

import cats.implicits._
import scodec.bits.ByteVector
import scodec.{Attempt, Codec, Decoder, Encoder}

import scala.language.implicitConversions

/**
 * Typeclass for encoding a value using a stable encoder/decoder scheme.  This scheme must never changed;
 * new schemes must instead be created.
 *
 * @tparam T the value that this typeclass is defined for
 */
trait ImmutableCodec[T] extends ImmutableEncoder[T] with ImmutableDecoder[T]

object ImmutableCodec {

  def apply[A](implicit instance: ImmutableCodec[A]): ImmutableCodec[A] = instance

  trait Ops[A] {
    def typeClassInstance: ImmutableCodec[A]
    def self: A
  }

  trait ToImmutableCodecOps {

    implicit def toSemigroupOps[A](target: A)(implicit tc: ImmutableCodec[A]): Ops[A] = new Ops[A] {
      val self = target
      val typeClassInstance = tc
    }
  }

  def fromScodecCodec[T: Codec]: ImmutableCodec[T] =
    new ImmutableCodec[T] {
      private val encoder = ImmutableEncoder.fromScodecEncoder[T]
      private val decoder = ImmutableDecoder.fromScodecDecoder[T]
      def immutableBytes(value:     T): ByteVector = encoder.immutableBytes(value)
      def fromImmutableBytes(bytes: ByteVector): Either[String, T] = decoder.fromImmutableBytes(bytes)
    }
}

trait ImmutableEncoder[T] {

  /**
   * Gets the byte representation of the value that should be persisted to a data store.
   * @param value the value to convert into bytes
   * @return an array of bytes representing the value which should then be persisted as-is
   */
  def immutableBytes(value: T): ByteVector
}

object ImmutableEncoder {

  def apply[A](implicit instance: ImmutableEncoder[A]): ImmutableEncoder[A] = instance

  trait Ops[A] {
    def typeClassInstance: ImmutableEncoder[A]
    def self: A
    def immutableBytes: ByteVector = typeClassInstance.immutableBytes(self)
  }

  trait ToImmutableEncoderOps {

    implicit def toImmutableEncoderOps[A](target: A)(implicit tc: ImmutableEncoder[A]): Ops[A] = new Ops[A] {
      val self: A = target
      val typeClassInstance: ImmutableEncoder[A] = tc
    }
  }

  def fromScodecEncoder[T: Encoder]: ImmutableEncoder[T] =
    t =>
      Encoder[T].encode(t) match {
        case Attempt.Successful(value) => value.toByteVector
        case Attempt.Failure(cause)    => throw new IllegalArgumentException(cause.messageWithContext)
      }
}

trait ImmutableDecoder[T] {

  /**
   * Attempts to decode a value of type `T` from a given array of bytes.
   * The given bytes should have been generated using the `immutableBytes` function.
   * @param bytes the immutable bytes to attempt to decode into a value of `T`
   * @return if successful, a value of type `T` represented by the input bytes, otherwise a failure message
   */
  def fromImmutableBytes(bytes: ByteVector): Either[String, T]

}

object ImmutableDecoder {

  def apply[A](implicit instance: ImmutableDecoder[A]): ImmutableDecoder[A] = instance

  trait Ops[A] {
    def typeClassInstance: ImmutableDecoder[A]
    def self: A
  }

  trait ToImmutableDecoderOps {

    implicit def toImmutableDecoderOps[A](target: A)(implicit tc: ImmutableDecoder[A]): Ops[A] = new Ops[A] {
      val self = target
      val typeClassInstance = tc
    }
  }

  def fromScodecDecoder[T: Decoder]: ImmutableDecoder[T] =
    t => Decoder[T].decodeValue(t.toBitVector).toEither.leftMap(e => e.messageWithContext)

  class BytesImmutableDecoderOps(private val bytes: ByteVector) extends AnyVal {

    def decodeImmutable[T: ImmutableDecoder]: Either[String, T] =
      ImmutableDecoder[T].fromImmutableBytes(bytes)
  }

  trait ToExtensionOps {

    implicit def immutableFromBytes(value: ByteVector): BytesImmutableDecoderOps =
      new BytesImmutableDecoderOps(value)
  }
}
