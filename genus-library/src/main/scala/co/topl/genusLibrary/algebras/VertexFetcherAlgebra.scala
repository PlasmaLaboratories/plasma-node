package co.topl.genusLibrary.algebras

import co.topl.consensus.models.BlockId
import co.topl.genusLibrary.model.GenusException
import com.tinkerpop.blueprints.Vertex

/**
 * Vertex finder on the stored Ledger
 * @tparam F the effect-ful context to retrieve the value in
 */
trait VertexFetcherAlgebra[F[_]] {

  /**
   * Fetch a BlockHeader vertex on the stored Ledger
   * @param blockId  blockId filter by field
   * @return Optional header vertex, None if it was not found
   */
  def fetchHeader(blockId: BlockId): F[Either[GenusException, Option[Vertex]]]

  /**
   * Fetch a BlockBody Vertex, which depends on header Vertex the stored Ledger, using the link to BlockHeader defined in the schema
   *
   * @param headerVertex filter by field
   * @return Optional body vertex, None if it was not found
   */
  def fetchBody(headerVertex: Vertex): F[Either[GenusException, Option[Vertex]]]

  /**
   * Fetch Transactions Vertices, which depends on header Vertex the stored Ledger, using the link to BlockHeader defined in the schema
   *
   * @param headerVertex filter by field
   * @return transactions vertices
   */
  def fetchTransactions(headerVertex: Vertex): F[Either[GenusException, Iterable[Vertex]]]

}
