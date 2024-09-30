package xyz.stratalab.numerics.algebras

import xyz.stratalab.models.utility.Ratio

trait Log1p[F[_]] {
  def evaluate(x: Ratio): F[Ratio]
}
