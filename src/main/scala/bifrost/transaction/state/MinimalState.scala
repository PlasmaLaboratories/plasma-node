package bifrost.transaction.state

import bifrost.transaction._
import bifrost.transaction.box.Box
import bifrost.transaction.box.proposition.Proposition
import bifrost.transaction.state.MinimalState.VersionTag
import bifrost.{NodeViewComponent, NodeViewModifier, PersistentNodeViewModifier}

import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */

trait MinimalState[P <: Proposition,
BX <: Box[P],
TX <: Transaction[P],
M <: PersistentNodeViewModifier[P, TX],
MS <: MinimalState[P, BX, TX, M, MS]] extends NodeViewComponent {
  self: MS =>

  def version: VersionTag

  def validate(transaction: TX): Try[Unit]

  def validate(mod: M): Try[Unit] = Try(mod.transactions.getOrElse(Seq()).foreach(tx => validate(tx).get))

  def isValid(tx: TX): Boolean = validate(tx).isSuccess

  def filterValid(txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def closedBox(boxId: Array[Byte]): Option[BX]

  def boxesOf(proposition: P): Seq[BX]

  def changes(mod: M): Try[StateChanges[P, BX]]

  def applyChanges(changes: StateChanges[P, BX], newVersion: VersionTag): Try[MS]

  def applyModifier(mod: M): Try[MS] = {
    validate(mod) flatMap { r =>
      changes(mod).flatMap(cs => applyChanges(cs, mod.id))
    }
  }

  def applyModifiers(mods: Seq[M]): Try[MS] =
    mods.foldLeft(Try(this)) { case (curTry, mod) =>
      curTry flatMap (_.applyModifier(mod))
    }

  def rollbackTo(version: VersionTag): Try[MS]
}

object MinimalState {
  type VersionTag = NodeViewModifier.ModifierId
}