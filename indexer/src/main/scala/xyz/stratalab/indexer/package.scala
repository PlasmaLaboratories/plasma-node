package org.plasmalabs

import cats.ApplicativeThrow
import cats.implicits.catsSyntaxApplicativeError
import io.grpc.{Status, StatusException}
import org.plasmalabs.indexer.model.{GE, GEs}

package object indexer {

  implicit class ThrowableAdapter(throwable: Throwable) {

    private def asException(status: Status, e: GE): StatusException =
      status
        .withCause(e.getCause)
        .augmentDescription(s"${e.getMessage}")
        .asException()

    def asGrpcException: StatusException =
      throwable match {
        case e: GEs.HeaderNotFound       => asException(Status.NOT_FOUND, e)
        case e: GEs.BodyNotFound         => asException(Status.NOT_FOUND, e)
        case e: GEs.TransactionsNotFound => asException(Status.NOT_FOUND, e)
        case e: GEs.NotFound             => asException(Status.NOT_FOUND, e)
        case e: GEs.UnImplemented.type   => asException(Status.UNIMPLEMENTED, e)
        case e: GEs.Internal             => asException(Status.INTERNAL, e)
        case e: GEs.InternalMessage      => asException(Status.INTERNAL, e)
        case e: GEs.InternalMessageCause => asException(Status.INTERNAL, e)
        case e                           => Status.fromThrowable(e).asException()
      }
  }

  implicit class FApplicativeErrorAdapter[F[_]: ApplicativeThrow, A](fa: F[A]) {

    def adaptErrorsToGrpc: F[A] =
      fa.adaptErr { case e =>
        e.asGrpcException
      }
  }
}
