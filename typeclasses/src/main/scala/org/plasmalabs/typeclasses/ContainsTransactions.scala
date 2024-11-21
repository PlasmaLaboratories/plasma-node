package org.plasmalabs.typeclasses

import cats.Foldable
import cats.data.ValidatedNec
import cats.implicits.*
import com.google.protobuf.ByteString
import org.plasmalabs.crypto.accumulators.LeafData
import org.plasmalabs.crypto.accumulators.merkle.MerkleTree
import org.plasmalabs.crypto.hash.digest.{Digest, Digest32, InvalidDigestFailure}
import org.plasmalabs.crypto.hash.{Blake2b, Blake2bHash}
import org.plasmalabs.models.*
import org.plasmalabs.models.utility.*
import org.plasmalabs.models.utility.HasLength.instances.*
import org.plasmalabs.models.utility.Lengths.*
import org.plasmalabs.node.models.FullBlockBody
import org.plasmalabs.sdk.models.TransactionId
import org.plasmalabs.sdk.models.transaction.IoTransaction
import org.plasmalabs.sdk.syntax.*

import scala.language.implicitConversions

trait ContainsTransactionIds[T] {
  def transactionIds(t: T): Seq[TransactionId]

  def merkleTreeOf(t: T): MerkleTree[Blake2b, Digest32] = {
    // The current MerkleTree implementation will, by default, use a shared digest and hash instance,
    // which introduces thread-safety issues.  We need to create a new instance for each call to avoid it.
    implicit val digest: Digest[Digest32] = new Digest[Digest32] {
      override def size: Int = Digest32.size

      override def from(bytes: Array[Byte]): ValidatedNec[InvalidDigestFailure, Digest32] = Digest32.validated(bytes)

      override def bytes(d: Digest32): Array[Byte] = d.value
    }
    implicit val hash: Blake2bHash[Digest32] = new Blake2bHash[Digest32] {}
    MerkleTree[Blake2b, Digest32](transactionIds(t).map(id => LeafData(id.value.toByteArray)))
  }

  def merkleTreeRootHashOf(t: T): TxRoot =
    Sized.strictUnsafe[Bytes, Lengths.`32`.type](ByteString.copyFrom(merkleTreeOf(t).rootHash.value))
}

object ContainsTransactionIds {

  def apply[A](implicit instance: ContainsTransactionIds[A]): ContainsTransactionIds[A] = instance

  trait Ops[A] {
    def typeClassInstance: ContainsTransactionIds[A]
    def self: A
    def transactionIds: Seq[TransactionId] = typeClassInstance.transactionIds(self)
    def merkleTree: MerkleTree[Blake2b, Digest32] = typeClassInstance.merkleTreeOf(self)
    def merkleTreeRootHash: TxRoot = typeClassInstance.merkleTreeRootHashOf(self)

  }

  trait ToContainsTransactionIdsOps {

    implicit def toContainsTransactionIdsOps[A](target: A)(implicit tc: ContainsTransactionIds[A]): Ops[A] =
      new Ops[A] {
        val self: A = target
        val typeClassInstance: ContainsTransactionIds[A] = tc
      }
  }

  trait Instances {

    implicit val blockNodeBody: ContainsTransactionIds[org.plasmalabs.node.models.BlockBody] = _.allTransactionIds

    implicit def containsTxToContainTxsId[G: ContainsTransactions]: ContainsTransactionIds[G] = txs =>
      implicitly[ContainsTransactions[G]].transactionsOf(txs).map(_.id)
  }

  object Instances extends Instances
}

/**
 * Satisfies that T contains transactions
 */
trait ContainsTransactions[T] {
  def transactionsOf(t: T): Seq[IoTransaction]

  def bloomFilterOf(@annotation.nowarn t: T): BloomFilter =
    // TODO
    Sized.strictUnsafe[Bytes, Lengths.`256`.type](ByteString.copyFrom(Array.fill[Byte](256)(1)))
}

object ContainsTransactions {

  def apply[A](implicit instance: ContainsTransactions[A]): ContainsTransactions[A] = instance

  trait Ops[A] {
    def typeClassInstance: ContainsTransactions[A]
    def self: A
    def bloomFilter: BloomFilter = typeClassInstance.bloomFilterOf(self)
  }

  trait ToContainsTransactionsOps {

    implicit def toContainsTransactionsOps[A](target: A)(implicit tc: ContainsTransactions[A]): Ops[A] = new Ops[A] {
      val self: A = target
      val typeClassInstance: ContainsTransactions[A] = tc
    }
  }

  trait Instances {

    implicit val fullBlockBodyContainsTransactions: ContainsTransactions[FullBlockBody] = _.allTransactions

    implicit val transactionsContainsTransactions: ContainsTransactions[Seq[IoTransaction]] = identity

    implicit def transactionsFoldableContainsTransactions[G[_]: Foldable]: ContainsTransactions[G[IoTransaction]] =
      t => t.toIterable.toSeq

  }

  object Instances extends Instances
}
