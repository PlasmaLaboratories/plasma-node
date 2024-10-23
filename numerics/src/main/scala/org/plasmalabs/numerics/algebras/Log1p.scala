package org.plasmalabs.numerics.algebras

import org.plasmalabs.models.utility.Ratio

trait Log1p[F[_]] {
  def evaluate(x: Ratio): F[Ratio]
}
