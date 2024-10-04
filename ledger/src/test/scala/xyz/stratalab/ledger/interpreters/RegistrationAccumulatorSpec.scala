package xyz.stratalab.ledger.interpreters

import cats.effect.IO
import cats.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import xyz.stratalab.algebras.testInterpreters.TestStore
import xyz.stratalab.consensus.models.{BlockId, StakingAddress}
import xyz.stratalab.eventtree.ParentChildTree
import xyz.stratalab.models.ModelGenerators._
import xyz.stratalab.models.generators.consensus.ModelGenerators._
import xyz.stratalab.node.models.BlockBody
import xyz.stratalab.numerics.implicits._
import xyz.stratalab.sdk.constants.NetworkConstants
import xyz.stratalab.sdk.models.box.{Attestation, Lock, Value}
import xyz.stratalab.sdk.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import xyz.stratalab.sdk.models.{Datum, LockAddress}
import xyz.stratalab.sdk.syntax._
import xyz.stratalab.typeclasses.implicits._

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
              tx1.id.outputAddress(0, 0, 0),
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
              tx2.id.outputAddress(0, 0, 0),
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
          ).apply _,
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
