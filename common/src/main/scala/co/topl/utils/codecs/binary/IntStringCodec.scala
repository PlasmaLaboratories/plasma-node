package co.topl.utils.codecs.binary

import co.topl.utils.Extensions.LongOps
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

object IntStringCodec {

  val maxBytes: Int = Int.MaxValue

  private val maxBitSize: Long = maxBytes.toLong * byteSize

  def decode(from: BitVector): Attempt[DecodeResult[IntString]] =
    UIntCodec.decode(from).flatMap { result =>
      val stringBitSize = result.value.toIntExact * byteSize

      if (result.remainder.length < stringBitSize)
        Attempt.failure(Err.insufficientBits(stringBitSize, result.remainder.length))
      else {
        val (stringBits, remaining) = result.remainder.splitAt(stringBitSize)

        val stringBytes = stringBits.toByteArray
        val stringBytesLength = stringBytes.length

        if (stringBytesLength <= maxBytes)
          Attempt.successful(DecodeResult(new String(stringBytes, stringCharacterSet), remaining))
        else
          Attempt.failure(Err("IntString value is outside of valid range."))
      }
    }

  def encode(value: IntString): Attempt[BitVector] = {
    val byteRepr = value.getBytes(stringCharacterSet)

    val byteLength = byteRepr.length

    if (byteLength <= maxBytes) UIntCodec.encode(byteLength).map(_ ++ BitVector(byteRepr))
    else Attempt.failure(Err("IntString value is outside of valid range."))
  }

  val codec: Codec[IntString] = new Codec[IntString] {
    override def decode(bits: BitVector): Attempt[DecodeResult[IntString]] = IntStringCodec.decode(bits)

    override def encode(value: IntString): Attempt[BitVector] = IntStringCodec.encode(value)

    override def sizeBound: SizeBound = UIntCodec.codec.sizeBound + SizeBound.atMost(maxBitSize)
  }

  trait Codecs {
    val intString: Codec[IntString] = codec
  }

  trait Implicits {
    implicit val intStringImplicitCodec: Codec[IntString] = codec
  }
}
