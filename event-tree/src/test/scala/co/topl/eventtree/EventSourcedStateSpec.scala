package co.topl.eventtree

import cats.data.OptionT
import cats.effect.{IO, Sync}
import cats.implicits._
import cats.{Functor, MonadThrow, Semigroupal}
import co.topl.algebras.Store
import co.topl.algebras.testInterpreters.TestStore
import co.topl.models._
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}

class EventSourcedStateSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  type F[A] = IO[A]
  import LedgerTreeTestSupport._

  test("traverse events forwards and backwards to provide the correct state along a tree") {
    for {
      eventStore <- TestStore.make[F, TypedIdentifier, Tx]
      deltaStore <- TestStore.make[F, TypedIdentifier, LedgerUnapply]
      treeStore  <- TestStore.make[F, TypedIdentifier, (Long, TypedIdentifier)]
      tree       <- ParentChildTree.FromStore.make(treeStore)
      _ <- txData.traverse { case (txId, from, amount, to) => eventStore.put(txId, Tx(txId, (from, amount), to)) }
      _ <- txAssociations.traverse { case (c, p) => tree.associate(c.asTxId, p.asTxId) }
      ledgerStore <- TestStore.make[F, TypedIdentifier, Bytes]
      initialEventId = "-1".asTxId
      _ <- ledgerStore.put(Ledger.Eval.CurrentEventIdId, initialEventId.allBytes)
      ledger = Ledger.Eval.make[F](ledgerStore)
      _ <- ledger.modifyBalanceOf("alice", _ => 100L.some.pure[F])
      eventTree <- EventSourcedState.OfTree
        .make[F, Ledger[F]](
          initialState = Sync[F].delay(ledger),
          initialEventId = Sync[F].delay(initialEventId),
          applyEvent = (ledger, txId) =>
            for {
              tx              <- OptionT(eventStore.get(txId)).getOrElse(???)
              previousBalance <- OptionT(ledger.balanceOf(tx.from._1)).getOrElse(???)
              _               <- ledger.modifyBalanceOf(tx.from._1, b => (b.getOrElse(0L) - tx.from._2).some.pure[F])
              _               <- ledger.modifyBalanceOf(tx.to, b => (b.getOrElse(0L) + tx.from._2).some.pure[F])
              currentTxId     <- ledger.eventId
              _               <- deltaStore.put(txId, LedgerUnapply(currentTxId, previousBalance, tx))
              _               <- ledger.setEventId(tx.id)
            } yield ledger,
          unapplyEvent = (ledger, txId) =>
            for {
              unapply <- OptionT(deltaStore.get(txId)).getOrElse(???)
              _       <- ledger.modifyBalanceOf(unapply.tx.from._1, _ => unapply.senderPreviousBalance.some.pure[F])
              _       <- ledger.modifyBalanceOf(unapply.tx.to, b => (b.getOrElse(0L) - unapply.tx.from._2).some.pure[F])
              _       <- ledger.setEventId(unapply.previousTxId)
            } yield ledger,
          parentChildTree = tree
        )
      ledgerC1 <- eventTree.stateAt("c1".asTxId)
      _        <- ledgerC1.balanceOf("alice").map(_.get).assertEquals(75L)
      _        <- ledgerC1.balanceOf("bob").map(_.get).assertEquals(15L)
      _        <- ledgerC1.balanceOf("chelsea").map(_.get).assertEquals(10L)

      ledgerC2 <- eventTree.stateAt("c2".asTxId)
      _        <- ledgerC2.balanceOf("alice").map(_.get).assertEquals(75L)
      _        <- ledgerC2.balanceOf("bob").map(_.get).assertEquals(10L)
      _        <- ledgerC2.balanceOf("chelsea").map(_.get).assertEquals(15L)

      ledgerE1D1C1 <- eventTree.stateAt("e1d1c1".asTxId)
      _            <- ledgerE1D1C1.balanceOf("alice").map(_.get).assertEquals(80L)
      _            <- ledgerE1D1C1.balanceOf("bob").map(_.get).assertEquals(10L)
      _            <- ledgerE1D1C1.balanceOf("chelsea").map(_.get).assertEquals(10L)
    } yield ()
  }
}

trait Ledger[F[_]] {
  def eventId: F[TypedIdentifier]
  def setEventId(id:        TypedIdentifier): F[Unit]
  def balanceOf(name:       String): F[Option[Long]]
  def modifyBalanceOf(name: String, f: Option[Long] => F[Option[Long]]): F[Unit]
}

object Ledger {

  object Eval {
    val CurrentEventIdId = TypedBytes(-1: Byte, Bytes(-1: Byte))

    def make[F[_]: MonadThrow](store: Store[F, TypedIdentifier, Bytes]): Ledger[F] = new Ledger[F] {

      def eventId: F[TypedIdentifier] =
        OptionT(store.get(CurrentEventIdId)).foldF(
          (new NoSuchElementException(CurrentEventIdId.show).raiseError[F, TypedIdentifier])
        )(TypedBytes(_).pure[F])

      def setEventId(id: TypedIdentifier): F[Unit] =
        store.put(CurrentEventIdId, id.allBytes)

      def balanceOf(name: String): F[Option[Long]] =
        OptionT
          .fromOption[F](Bytes.encodeUtf8(name).toOption)
          .map(TypedBytes(1: Byte, _))
          .flatMapF(store.get)
          .map(_.toLong())
          .value

      def modifyBalanceOf(name: String, f: Option[Long] => F[Option[Long]]): F[Unit] =
        OptionT
          .fromOption[F](Bytes.encodeUtf8(name).toOption)
          .map(TypedBytes(1: Byte, _))
          .getOrElseF(???)
          .flatTap(key =>
            OptionT(store.get(key))
              .map(_.toLong())
              .flatTransform(f)
              .foldF(store.remove(key))(t => store.put(key, Bytes.fromLong(t)))
          )
          .void
    }
  }
}

object LedgerTreeTestSupport {

  case class Tx(id: TypedIdentifier, from: (String, Long), to: String)

  val txData = List(
    ("a".asTxId, "alice", 10L, "bob"),
    ("b".asTxId, "alice", 10L, "chelsea"),
    ("c1".asTxId, "alice", 5L, "bob"),
    ("c2".asTxId, "alice", 5L, "chelsea"),
    ("d1c1".asTxId, "bob", 5L, "chelsea"),
    ("e1d1c1".asTxId, "chelsea", 5L, "alice")
  )

  val txAssociations = List(
    "e1d1c1" -> "d1c1",
    "d1c1"   -> "c1",
    "c1"     -> "b",
    "b"      -> "a",
    "a"      -> "-1",
    "c2"     -> "b"
  )

  case class LedgerUnapply(previousTxId: TypedIdentifier, senderPreviousBalance: Long, tx: Tx)

  def printLedgerBalances[F[_]: Functor: Semigroupal](ledger: Ledger[F]): F[Unit] =
    (ledger.balanceOf("alice"), ledger.balanceOf("bob"), ledger.balanceOf("chelsea")).mapN(
      (aBalance, bBalance, cBalance) => println(show"Balances: alice=$aBalance bob=$bBalance chelsea=$cBalance")
    )

  implicit class StringOps(string: String) {
    def asTxId: TypedIdentifier = TypedBytes(1: Byte, Bytes(string.getBytes()))
  }

  implicit class CatsIOOps[T](io: IO[T]) {
    def value: T = io.unsafeRunSync()(cats.effect.unsafe.IORuntime.global)
  }
}
