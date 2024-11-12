package org.plasmalabs.numerics

import org.plasmalabs.models.utility.Ratio
import org.plasmalabs.quivr.models.{Int128, Ratio => QuivrRatio}

import scala.language.implicitConversions

trait NumberOps {
  import org.plasmalabs.sdk.syntax._

  implicit def intAsInt128(int: Int): Int128 =
    BigInt(int)

  implicit def protoRatioToRatio(ratio: QuivrRatio): Ratio =
    Ratio(ratio.numerator: BigInt, ratio.denominator: BigInt)

  implicit def ratioToProtoRatio(ratio: Ratio): QuivrRatio =
    QuivrRatio(ratio.numerator: Int128, ratio.denominator: Int128)
}
