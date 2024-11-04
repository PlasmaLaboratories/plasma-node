package org.plasmalabs.indexer.model

import com.google.protobuf.ByteString
import org.plasmalabs.consensus.models.BlockId
import org.plasmalabs.sdk.models.TransactionId

import scala.collection.immutable.ListSet

class GETest extends munit.FunSuite {

  test("Indexer Exception Message") {
    val msg = "boom!"
    val exception = GEs.InternalMessage(msg)
    assertEquals(exception.getMessage, msg, "message should be correct")
  }

  test("Indexer Exception with cause") {
    val msg = "boom!"
    val cause = new RuntimeException()
    val exception = GEs.InternalMessageCause(msg, cause)
    assertEquals(exception.getMessage, msg, "message should be correct")
    assert(cause eq exception.getCause, "Cause should be correct")
  }

  test("Indexer Exception Internal") {
    interceptMessage[GE]("boom!") {
      throw GEs.Internal(new RuntimeException("boom!"))
    }
  }

  test("Indexer Exception UnImplemented") {
    interceptMessage[GE]("An implementation is missing") {
      throw GEs.UnImplemented
    }
  }

  test("Indexer Exception NotFound") {
    interceptMessage[GE]("boom!") {
      throw GEs.NotFound("boom!")
    }
  }

  test("Indexer Exception HeaderNotFound") {
    val blockId = BlockId.of(value = ByteString.copyFrom(Array.fill[Byte](32)(0)))
    val expected = "Block header wasn't found. BlockId=[b_11111111111111111111111111111111]"
    interceptMessage[GE](expected) {
      throw GEs.HeaderNotFound(blockId)
    }
  }

  test("Indexer Exception BodyNotFound") {
    val blockId = BlockId.of(value = ByteString.copyFrom(Array.fill[Byte](32)(0)))
    val expected = "Block body wasn't found. BlockId=[b_11111111111111111111111111111111]"
    interceptMessage[GE](expected) {
      throw GEs.BodyNotFound(blockId)
    }
  }

  test("Indexer Exception TransactionsNotFound") {
    val ioTxId_1 =
      TransactionId(ByteString.copyFrom(Array.fill[Byte](32)(1)))

    val ioTxId_2 =
      TransactionId(ByteString.copyFrom(Array.fill[Byte](32)(2)))

    val ioTx32s = ListSet.from(ListSet(ioTxId_1, ioTxId_2))

    val expected =
      "Transactions weren't found. Transactions=[ListSet(t_4vJ9JU1bJJE96FWSJKvHsmmFADCg4gpZQff4P3bkLKi, t_8qbHbw2BbbTHBW1sbeqakYXVKRQM8Ne7pLK7m6CVfeR)]"

    interceptMessage[GE](expected) {
      throw GEs.TransactionsNotFound(ioTx32s)
    }
  }
}
