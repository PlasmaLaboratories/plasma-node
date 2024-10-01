package xyz.stratalab.numerics.algebras

import xyz.stratalab.models.utility.Ratio

trait RationalApproximation[F[_]] {
  def rationalApproximation(x: Ratio): F[Ratio]
}
