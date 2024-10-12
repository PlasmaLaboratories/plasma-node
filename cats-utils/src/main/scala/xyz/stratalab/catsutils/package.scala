package xyz.stratalab

import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.~>

import scala.concurrent.Future

package object catsutils extends FOps with AsFS2StreamOps {

  implicit def ioToFuture(implicit ioRuntime: IORuntime): IO ~> Future =
    FunctionK.lift[IO,Future]([X] => (_:IO[X]).unsafeToFuture())
}
