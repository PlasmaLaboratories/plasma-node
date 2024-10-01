package xyz.stratalab.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.consensus.models.BlockId
import org.apache.commons.lang3.exception.ExceptionUtils
import xyz.stratalab.consensus.models.BlockHeaderValidationFailure
import xyz.stratalab.ledger.models.BodyValidationError
import xyz.stratalab.models.p2p._

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
