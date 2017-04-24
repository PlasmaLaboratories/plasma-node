package examples.bifrost.mempool

import examples.bifrost.transaction.{BifrostTransaction, StableCoinTransfer}
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.transaction.MemoryPool
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success, Try}


case class BifrostMemPool(unconfirmed: TrieMap[ByteArrayWrapper, BifrostTransaction])
  extends MemoryPool[BifrostTransaction, BifrostMemPool] with ScorexLogging {
  override type NVCT = BifrostMemPool

  private def key(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(id)
  private val boxesInMempool = new TrieMap[ByteArrayWrapper, ByteArrayWrapper]()

  //getters
  override def getById(id: ModifierId): Option[BifrostTransaction] =
  unconfirmed.get(key(id))

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(key(id))

  override def getAll(ids: Seq[ModifierId]): Seq[BifrostTransaction] = ids.flatMap(getById)

  //modifiers
  override def put(tx: BifrostTransaction): Try[BifrostMemPool] = Try {
    unconfirmed.put(key(tx.id), tx)
    tx.boxIdsToOpen.foreach(boxId => {
      println(s"${Console.RED} Found Duplicate key ${Base58.encode(boxId)} ${Console.RESET}")
      val exists = boxesInMempool.get(key(boxId)).isDefined
      require(!exists)
    })
    tx.boxIdsToOpen.foreach(boxId => {
      boxesInMempool.put(key(boxId), key(boxId))
    })
    println(s"${Console.CYAN}boxesInMempool ${boxesInMempool.keys.map(k => Base58.encode(k.data))} ${Console.RESET}")
    this
  }

  //todo
  override def put(txs: Iterable[BifrostTransaction]): Try[BifrostMemPool] = Success(putWithoutCheck(txs))

  override def putWithoutCheck(txs: Iterable[BifrostTransaction]): BifrostMemPool = {
    txs.foreach(tx => unconfirmed.put(key(tx.id), tx))
    txs.foreach(tx => {
      tx.boxIdsToOpen.map(boxId => { boxesInMempool.put(key(boxId), key(boxId)) })
    })
    this
  }

  override def remove(tx: BifrostTransaction): BifrostMemPool = {
    unconfirmed.remove(key(tx.id))
    this
  }

  override def take(limit: Int): Iterable[BifrostTransaction] =
    unconfirmed.values.toSeq.sortBy(-_.fee).take(limit)

  override def filter(condition: (BifrostTransaction) => Boolean): BifrostMemPool = {
    unconfirmed.retain { (k, v) =>
      condition(v)
    }
    this
  }

  override def size: Int = unconfirmed.size
}


object BifrostMemPool {
  lazy val emptyPool: BifrostMemPool = BifrostMemPool(TrieMap())
}