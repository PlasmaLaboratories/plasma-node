package org.plasmalabs.numerics.algebras

import org.plasmalabs.models.utility.Ratio

trait RationalApproximation[F[_]] {
  def rationalApproximation(x: Ratio): F[Ratio]
}
