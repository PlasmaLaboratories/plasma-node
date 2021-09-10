package co.topl.utils.codecs.binary

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err, SizeBound}

import java.util

object ULongCodec {

  private val `0x7f` = BitVector(0x7f)
  private val `0x80` = BitVector(0x80)
  private val `0 bitvector` = BitVector(0)

  def decode(from: BitVector): Attempt[DecodeResult[ULong]] = {
    var result: Long = 0
    var iteration = 0

    while (iteration < 10) {
      val bitPointer = iteration * byteSize

      if (from.length < bitPointer + byteSize)
        return Attempt.failure(Err.insufficientBits(bitPointer + byteSize, from.length))

      val b = from.slice(bitPointer, bitPointer + byteSize)

      result = result | ((b & `0x7f`).toLong() << iteration * 7)

      if ((b & `0x80`) === `0 bitvector`)
        return Attempt.successful(DecodeResult(result, from.drop(bitPointer + byteSize)))

      iteration += 1
    }

    Attempt.failure(Err("Unexpected bytes remaining."))
  }

  def encode(value: ULong): Attempt[BitVector] = {
    var output = BitVector.empty
    var runningValue = value

    while (true)
      if ((runningValue & ~0x7fL) == 0) {
        output = output ++ BitVector(runningValue.asInstanceOf[Byte])
        return Attempt.successful(output)
      } else {
        output = output ++ BitVector(((runningValue.asInstanceOf[Int] & 0x7f) | 0x80).toByte)
        runningValue >>>= 7
      }

    Attempt.successful(output)
  }

  val codec: Codec[ULong] = new Codec[ULong] {
    override def decode(bits: BitVector): Attempt[DecodeResult[ULong]] = ULongCodec.decode(bits)

    override def encode(value: ULong): Attempt[BitVector] = ULongCodec.encode(value)

    override def sizeBound: SizeBound = SizeBound.bounded(8, 64)
  }

  trait Codecs {
    val uLong: Codec[ULong] = codec
  }

  trait Implicits {
    val uLongImplicitCodec: Codec[ULong] = codec
  }

}
