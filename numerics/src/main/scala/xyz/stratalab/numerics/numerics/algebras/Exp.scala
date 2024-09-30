package xyz.stratalab.numerics.algebras

import xyz.stratalab.models.utility.Ratio

trait Exp[F[_]] {
  def evaluate(x: Ratio): F[Ratio]
}
