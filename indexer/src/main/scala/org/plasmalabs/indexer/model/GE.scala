package org.plasmalabs.indexer.model

import cats.implicits.*
import org.plasmalabs.consensus.models.BlockId
import org.plasmalabs.sdk.models.TransactionId
import org.plasmalabs.typeclasses.implicits.*

import scala.collection.immutable.ListSet

/**
 * Indexer Exception
 * Base exception class for the Indexer library.
 * @param message the detail message (which is saved for later retrieval by the getMessage() method).
 */
abstract class GE(message: String) extends Exception(message)

object GEs {

  case class HeaderNotFound(blockId: BlockId) extends GE(s"Block header wasn't found. BlockId=[${blockId.show}]")

  case class BodyNotFound(blockId: BlockId) extends GE(s"Block body wasn't found. BlockId=[${blockId.show}]")

  case class TransactionsNotFound(ioTx32s: ListSet[TransactionId])
      extends GE(s"Transactions weren't found. Transactions=[${ioTx32s.map(_.show)}]")

  case class NotFound(msg: String) extends GE(msg)

  case object UnImplemented extends GE("An implementation is missing")

  case class Internal(cause: Throwable) extends GE(s"${cause.getMessage}") {
    this.initCause(cause)
  }

  case class InternalMessage(msg: String) extends GE(msg)

  case class InternalMessageCause(msg: String, cause: Throwable) extends GE(msg) {
    this.initCause(cause)
  }
}
