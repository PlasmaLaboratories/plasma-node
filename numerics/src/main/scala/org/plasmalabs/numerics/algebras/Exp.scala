package org.plasmalabs.numerics.algebras

import org.plasmalabs.models.utility.Ratio

trait Exp[F[_]] {
  def evaluate(x: Ratio): F[Ratio]
}
