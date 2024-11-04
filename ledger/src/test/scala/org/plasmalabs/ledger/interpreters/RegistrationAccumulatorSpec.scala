package org.plasmalabs.ledger.interpreters

import cats.effect.IO
import cats.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.plasmalabs.algebras.testInterpreters.TestStore
import org.plasmalabs.consensus.models.{BlockId, StakingAddress}
import org.plasmalabs.eventtree.ParentChildTree
import org.plasmalabs.models.ModelGenerators._
import org.plasmalabs.models.generators.consensus.ModelGenerators._
import org.plasmalabs.node.models.BlockBody
import org.plasmalabs.numerics.implicits._
import org.plasmalabs.sdk.constants.NetworkConstants
import org.plasmalabs.sdk.models.box.{Attestation, Lock, Value}
import org.plasmalabs.sdk.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import org.plasmalabs.sdk.models.{Datum, LockAddress}
import org.plasmalabs.sdk.syntax._
import org.plasmalabs.typeclasses.implicits._
import org.scalamock.munit.AsyncMockFactory

class RegistrationAccumulatorSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  val lock: Lock = Lock().withPredicate(Lock.Predicate())

  val lockAddress: LockAddress = lock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)

  test("accumulate a set of staking addresses across multiple blocks") {
    val genesisBlockId = arbitraryBlockId.arbitrary.first
    val blockId0 = arbitraryBlockId.arbitrary.first
    val blockId1 = arbitraryBlockId.arbitrary.first
    val blockId2 = arbitraryBlockId.arbitrary.first
    val registration0 = arbitraryStakingRegistration.arbitrary.first
    val tx0 =
      IoTransaction(datum = Datum.IoTransaction.defaultInstance)
        .withOutputs(
          List(UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(5, registration0.some))))
        )
        .embedId
    val registration1 = arbitraryStakingRegistration.arbitrary.first
    val tx1 =
      IoTransaction(datum = Datum.IoTransaction.defaultInstance)
        .withOutputs(
          List(UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(5, registration1.some))))
        )
        .embedId
    val registration2 = arbitraryStakingRegistration.arbitrary.first
    val tx2 =
      IoTransaction(datum = Datum.IoTransaction.defaultInstance)
        .withInputs(
          List(
            SpentTransactionOutput(
              tx1.id.outputAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID, 0),
              Attestation().withPredicate(Attestation.Predicate.defaultInstance),
              tx1.outputs(0).value
            )
          )
        )
        .withOutputs(
          List(UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(5, registration2.some))))
        )
        .embedId

    val registration3 = arbitraryStakingRegistration.arbitrary.first
    val tx3 =
      IoTransaction(datum = Datum.IoTransaction.defaultInstance)
        .withInputs(
          List(
            SpentTransactionOutput(
              tx2.id.outputAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID, 0),
              Attestation().withPredicate(Attestation.Predicate.defaultInstance),
              tx2.outputs(0).value
            )
          )
        )
        .withOutputs(
          List(UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(5, registration3.some))))
        )
        .embedId

    val testResource =
      for {
        parentChildTree <- ParentChildTree.FromRef.make[IO, BlockId].toResource
        _               <- parentChildTree.associate(blockId0, genesisBlockId).toResource
        _               <- parentChildTree.associate(blockId1, blockId0).toResource
        _               <- parentChildTree.associate(blockId2, blockId1).toResource
        (underTest, _) <- RegistrationAccumulator.make[IO](
          genesisBlockId.pure[IO],
          Map(
            blockId0 -> BlockBody(List(tx0.id)).pure[IO],
            blockId1 -> BlockBody(List(tx1.id, tx2.id)).pure[IO],
            blockId2 -> BlockBody(List(tx3.id)).pure[IO]
          ),
          Map(
            tx0.id -> tx0.pure[IO],
            tx1.id -> tx1.pure[IO],
            tx2.id -> tx2.pure[IO],
            tx3.id -> tx3.pure[IO]
          ),
          parentChildTree,
          _ => IO.unit,
          TestStore.make[IO, StakingAddress, Unit].widen
        )
        _ <- underTest.contains(blockId0)(registration0.address).assert.toResource
        _ <- underTest.contains(blockId0)(registration1.address).map(!_).assert.toResource
        _ <- underTest.contains(blockId1)(registration1.address).map(!_).assert.toResource
        _ <- underTest.contains(blockId1)(registration2.address).assert.toResource
        _ <- underTest.contains(blockId2)(registration2.address).map(!_).assert.toResource
        _ <- underTest.contains(blockId2)(registration3.address).assert.toResource
        _ <- underTest.contains(blockId1)(registration2.address).assert.toResource
        _ <- underTest.contains(blockId0)(registration0.address).assert.toResource
        _ <- underTest.contains(blockId0)(registration1.address).map(!_).assert.toResource
      } yield ()

    testResource.use_
  }

}
