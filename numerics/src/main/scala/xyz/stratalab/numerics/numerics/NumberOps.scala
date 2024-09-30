package xyz.stratalab.numerics

import quivr.models.Int128
import xyz.stratalab.models.utility.Ratio

import scala.language.implicitConversions

trait NumberOps {
  import co.topl.brambl.syntax._

  implicit def intAsInt128(int: Int): Int128 =
    BigInt(int)

  implicit def protoRatioToRatio(ratio: quivr.models.Ratio): Ratio =
    Ratio(ratio.numerator: BigInt, ratio.denominator: BigInt)

  implicit def ratioToProtoRatio(ratio: Ratio): quivr.models.Ratio =
    quivr.models.Ratio(ratio.numerator: Int128, ratio.denominator: Int128)
}
