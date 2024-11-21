package org.plasmalabs.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.implicits.*
import org.apache.commons.lang3.exception.ExceptionUtils
import org.plasmalabs.consensus.models.{BlockHeaderValidationFailure, BlockId}
import org.plasmalabs.ledger.models.BodyValidationError
import org.plasmalabs.models.p2p.*

sealed abstract class BlockApplyError extends Exception

object BlockApplyError {
  sealed abstract class HeaderApplyException extends BlockApplyError

  object HeaderApplyException {

    case class HeaderValidationException(blockId: BlockId, source: HostId, error: BlockHeaderValidationFailure)
        extends HeaderApplyException

    case class UnknownError(ex: Throwable) extends HeaderApplyException {
      this.initCause(ex)

      override def toString: String =
        show"Error applying block header due next throwable ${ex.toString} ${ExceptionUtils.getStackTrace(ex)}"
    }
  }

  sealed abstract class BodyApplyException extends BlockApplyError

  object BodyApplyException {

    case class BodyValidationException(blockId: BlockId, source: HostId, errors: NonEmptyChain[BodyValidationError])
        extends BodyApplyException

    case class UnknownError(ex: Throwable) extends BodyApplyException {
      this.initCause(ex)

      override def toString: String =
        show"Error applying block body due next throwable ${ex.toString} ${ExceptionUtils.getStackTrace(ex)}"
    }
  }
}
