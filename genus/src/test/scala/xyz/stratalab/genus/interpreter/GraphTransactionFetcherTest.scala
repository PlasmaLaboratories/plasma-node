package xyz.stratalab.genus.interpreter

import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import xyz.stratalab.sdk.generators.ModelGenerators._
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models.{LockAddress, TransactionId, TransactionInputAddress, TransactionOutputAddress}
import xyz.stratalab.sdk.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.consensus.models.BlockHeader
import co.topl.genus.services._
import com.tinkerpop.blueprints.Vertex
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import xyz.stratalab.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import xyz.stratalab.genus.algebras.VertexFetcherAlgebra
import xyz.stratalab.genus.interpreter.GraphTransactionFetcher
import xyz.stratalab.genus.model.{GE, GEs}
import xyz.stratalab.genus.orientDb.OrientThread
import xyz.stratalab.genus.orientDb.instances.{SchemaBlockHeader, SchemaIoTransaction, SchemaTxo}
import xyz.stratalab.models.generators.consensus.ModelGenerators._

class GraphTransactionFetcherTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("On fetchTransaction with throwable response, a FailureMessageWithCause should be returned") {

    PropF.forAllF { (transactionId: TransactionId) =>
      withMock {

        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          expectedTh = new IllegalStateException("boom!")
          _ = (vertexFetcher.fetchTransaction)
            .expects(transactionId)
            .once()
            .returning(
              (GEs
                .InternalMessageCause("GraphVertexFetcher:fetchTransaction", expectedTh): GE)
                .asLeft[Option[Vertex]]
                .pure[F]
            )
          graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            graphTransactionFetcher.fetchTransaction(transactionId),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchTransaction", expectedTh): GE)
              .asLeft[Option[IoTransaction]]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTransaction if an empty iterator is returned, None IoTransaction should be returned") {

    PropF.forAllF { (transactionId: TransactionId) =>
      withMock {

        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          _ = (vertexFetcher.fetchTransaction)
            .expects(transactionId)
            .once()
            .returning(Option.empty[Vertex].asRight[GE].pure[F])
          graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            graphTransactionFetcher.fetchTransaction(transactionId),
            Option.empty[IoTransaction].asRight[GE]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTransaction if IoTransactionVertex exist, Some IoTransaction should be returned") {

    PropF.forAllF { (transactionId: TransactionId, ioTransaction: IoTransaction) =>
      withMock {

        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          vertex                                   <- mock[Vertex].pure[F].toResource

          _ = (vertex.getProperty[Array[Byte]])
            .expects(SchemaIoTransaction.Field.Transaction)
            .once()
            .returning(ioTransaction.toByteArray)

          _ = (vertexFetcher.fetchTransaction)
            .expects(transactionId)
            .once()
            .returning(Option(vertex).asRight[GE].pure[F])

          graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            graphTransactionFetcher.fetchTransaction(transactionId),
            Some(ioTransaction).asRight[GE]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTransactionReceipt with throwable response, a FailureMessageWithCause should be returned") {

    PropF.forAllF { (transactionId: TransactionId) =>
      withMock {

        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          expectedTh = new IllegalStateException("boom!")
          _ = (vertexFetcher.fetchTransaction)
            .expects(transactionId)
            .once()
            .returning(
              (GEs
                .InternalMessageCause("GraphVertexFetcher:fetchTransaction", expectedTh): GE)
                .asLeft[Option[Vertex]]
                .pure[F]
            )
          graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            graphTransactionFetcher.fetchTransactionReceipt(transactionId),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchTransaction", expectedTh): GE)
              .asLeft[Option[TransactionReceipt]]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTransactionReceipt if an empty iterator is returned, None IoTransaction should be returned") {

    PropF.forAllF { (transactionId: TransactionId) =>
      withMock {

        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          _ = (vertexFetcher.fetchTransaction)
            .expects(transactionId)
            .once()
            .returning(Option.empty[Vertex].asRight[GE].pure[F])
          graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            graphTransactionFetcher.fetchTransactionReceipt(transactionId),
            Option.empty[TransactionReceipt].asRight[GE]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTransactionReceipt if IoTransactionVertex exist, Some TransactionReceipt should be returned") {

    PropF.forAllF { (transactionId: TransactionId, ioTransaction: IoTransaction, blockHeader: BlockHeader) =>
      withMock {

        val blockHeaderVertex = mock[Vertex]
        val iotxVertex = mock[Vertex]

        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource

          _ = (vertexFetcher.fetchTransaction)
            .expects(transactionId)
            .once()
            .returning(Option(iotxVertex).asRight[GE].pure[F])

          _ = (iotxVertex.getProperty[Vertex])
            .expects(SchemaIoTransaction.Field.ParentBlock)
            .once()
            .returning(blockHeaderVertex)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]])
            .expects(SchemaBlockHeader.Field.BlockId)
            .once()
            .returning(blockHeader.embedId.id.value.toByteArray)

          _ = (blockHeaderVertex
            .getProperty[java.lang.Long])
            .expects(SchemaBlockHeader.Field.ParentSlot)
            .once()
            .returning(blockHeader.parentSlot)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]])
            .expects(SchemaBlockHeader.Field.EligibilityCertificate)
            .once()
            .returning(blockHeader.eligibilityCertificate.toByteArray)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]])
            .expects(SchemaBlockHeader.Field.OperationalCertificate)
            .once()
            .returning(blockHeader.operationalCertificate.toByteArray)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]])
            .expects(SchemaBlockHeader.Field.Address)
            .once()
            .returning(blockHeader.address.toByteArray)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]])
            .expects(SchemaBlockHeader.Field.Metadata)
            .once()
            .returning(blockHeader.metadata.toByteArray)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]])
            .expects(SchemaBlockHeader.Field.TxRoot)
            .once()
            .returning(blockHeader.txRoot.toByteArray)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]])
            .expects(SchemaBlockHeader.Field.BloomFilter)
            .once()
            .returning(blockHeader.bloomFilter.toByteArray)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]])
            .expects(SchemaBlockHeader.Field.ParentHeaderId)
            .once()
            .returning(blockHeader.parentHeaderId.value.toByteArray)

          _ = (blockHeaderVertex
            .getProperty[java.lang.Long])
            .expects(SchemaBlockHeader.Field.Slot)
            .once()
            .returning(blockHeader.slot)

          _ = (blockHeaderVertex
            .getProperty[java.lang.Long])
            .expects(SchemaBlockHeader.Field.Height)
            .once()
            .returning(blockHeader.height)

          _ = (blockHeaderVertex
            .getProperty[java.lang.Long])
            .expects(SchemaBlockHeader.Field.Timestamp)
            .once()
            .returning(blockHeader.timestamp)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]])
            .expects(SchemaBlockHeader.Field.Version)
            .once()
            .returning(blockHeader.version.toByteArray)

          _ = (iotxVertex.getProperty[Array[Byte]])
            .expects(SchemaIoTransaction.Field.Transaction)
            .once()
            .returning(ioTransaction.toByteArray)

          expectedTransactionReceipt =
            TransactionReceipt(
              ioTransaction,
              ConfidenceFactor.defaultInstance,
              blockHeader.id,
              ChainDistance(blockHeader.height)
            )

          graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            graphTransactionFetcher.fetchTransactionReceipt(transactionId),
            Some(expectedTransactionReceipt).asRight[GE]
          ).toResource

        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTransactionsByAddress with throwable response, a FailureMessageWithCause should be returned") {

    PropF.forAllF { (lockAddress: LockAddress) =>
      withMock {

        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          expectedTh = new IllegalStateException("boom!")
          _ = (vertexFetcher.fetchLockAddress)
            .expects(lockAddress)
            .once()
            .returning(
              (GEs
                .InternalMessageCause("GraphVertexFetcher:fetchTransactionsByAddress", expectedTh): GE)
                .asLeft[Option[Vertex]]
                .pure[F]
            )
          graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            graphTransactionFetcher.fetchTransactionByLockAddress(lockAddress, TxoState.SPENT),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchTransactionsByAddress", expectedTh): GE)
              .asLeft[List[Txo]]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTransactionsByAddress with empty response, a empty seq should be returned") {

    PropF.forAllF { (lockAddress: LockAddress) =>
      withMock {

        val res = for {
          given OrientThread[F] <- OrientThread.create[F]
          vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          vertex = mock[Vertex]

          _ = (vertexFetcher.fetchLockAddress)
            .expects(lockAddress)
            .once()
            .returning(Option(vertex).asRight[GE].pure[F])

          _ = (vertexFetcher
            .fetchTxosByLockAddress(_: Vertex, _: TxoState))
            .expects(vertex, TxoState.SPENT)
            .once()
            .returning(List.empty[Vertex].asRight[GE].pure[F])

          graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            graphTransactionFetcher.fetchTransactionByLockAddress(lockAddress, TxoState.SPENT),
            List.empty[Txo].asRight[GE]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTransactionsByAddress with UNSPENT filter, a empty seq should be returned") {

    PropF.forAllF {
      (
        lockAddress: LockAddress
      ) =>
        withMock {

          val res = for {
            given OrientThread[F] <- OrientThread.create[F]
            vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
            lockAddressVertex = mock[Vertex]

            _ = (vertexFetcher.fetchLockAddress)
              .expects(lockAddress)
              .once()
              .returning(Option(lockAddressVertex).asRight[GE].pure[F])

            _ = (vertexFetcher
              .fetchTxosByLockAddress(_: Vertex, _: TxoState))
              .expects(lockAddressVertex, TxoState.SPENT)
              .once()
              .returning(List.empty[Vertex].asRight[GE].pure[F])

            graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
            _ <- assertIO(
              graphTransactionFetcher.fetchTransactionByLockAddress(lockAddress, TxoState.SPENT),
              List.empty[Txo].asRight[GE]
            ).toResource
          } yield ()

          res.use_
        }

    }
  }

  test("On fetchTransactionsByAddress with SPENT filter, a seq with 1 item should be returned") {

    PropF.forAllF {
      (
        lockAddress:              LockAddress,
        transactionOutputAddress: TransactionOutputAddress,
        transactionOutput:        UnspentTransactionOutput,
        spendingTransactionInput: SpentTransactionOutput
      ) =>
        withMock {

          val res = for {
            given OrientThread[F] <- OrientThread.create[F]
            vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
            lockAddressVertex = mock[Vertex]
            spendingTransactionVertex = mock[Vertex]
            txoVertex = mock[Vertex]
            spendingTransaction = IoTransaction.defaultInstance.withInputs(List(spendingTransactionInput))

            _ = (vertexFetcher.fetchLockAddress)
              .expects(lockAddress)
              .once()
              .returning(Option(lockAddressVertex).asRight[GE].pure[F])

            _ = (txoVertex.getProperty[Array[Byte]])
              .expects(SchemaTxo.Field.TransactionOutput)
              .once()
              .returning(transactionOutput.toByteArray)

            _ = (txoVertex.getProperty[java.lang.Integer])
              .expects(SchemaTxo.Field.State)
              .once()
              .returning(TxoState.SPENT.value)

            _ = (txoVertex.getProperty[java.lang.Integer])
              .expects(SchemaTxo.Field.SpendingInputIndex)
              .once()
              .returning(0)

            _ = (txoVertex.getProperty[Vertex])
              .expects(SchemaTxo.Field.SpendingTransaction)
              .once()
              .returning(spendingTransactionVertex)

            _ = (spendingTransactionVertex.getProperty[Array[Byte]])
              .expects(SchemaIoTransaction.Field.Transaction)
              .once()
              .returning(spendingTransaction.toByteArray)

            _ = (txoVertex.getProperty[Array[Byte]])
              .expects(SchemaTxo.Field.OutputAddress)
              .once()
              .returning(transactionOutputAddress.toByteArray)

            _ = (vertexFetcher
              .fetchTxosByLockAddress(_: Vertex, _: TxoState))
              .expects(lockAddressVertex, TxoState.SPENT)
              .once()
              .returning(List(txoVertex).asRight[GE].pure[F])

            graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
            txos <- EitherT(graphTransactionFetcher.fetchTransactionByLockAddress(lockAddress, TxoState.SPENT))
              .valueOrF(IO.raiseError)
              .toResource
            _ <- IO.pure(txos.length).assertEquals(1).toResource
            txo = txos.head
            _ <- IO
              .pure(txo.spender.get.inputAddress)
              .assertEquals(
                TransactionInputAddress(
                  network = spendingTransactionInput.address.network,
                  ledger = spendingTransactionInput.address.ledger,
                  index = 0,
                  id = spendingTransaction.id
                )
              )
              .toResource
            _ <- IO.pure(txo.spender.get.input).assertEquals(spendingTransactionInput).toResource
          } yield ()

          res.use_
        }

    }
  }

}
