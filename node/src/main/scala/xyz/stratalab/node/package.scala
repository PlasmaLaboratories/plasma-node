package xyz.stratalab

import cats.MonadThrow
import cats.implicits.toShow
import xyz.stratalab.blockchain.CurrentEventIdGetterSetters.Indices
import xyz.stratalab.blockchain.DataStores
import xyz.stratalab.consensus.models.BlockId
import xyz.stratalab.typeclasses.implicits.showBlockId

package object node {

  /**
   * For private testnets, it is convenient to run each network from its own directory, rather than re-using
   * a single directory over and over.  This method helps interpolate a directory
   * (i.e. /tmp/node/data/{genesisBlockId}) with the genesis block ID.
   */
  def interpolateBlockId(genesisBlockId: BlockId)(path: String): String =
    path.replace("{genesisBlockId}", genesisBlockId.show)

  implicit class DataStoresOps[F[_]: MonadThrow](dataStores: DataStores[F]) {
    def canonicalHead: F[BlockId] = dataStores.currentEventIds.getOrRaise(Indices.CanonicalHead)
  }

  // TODO move to other place?
  val initialVersion = 1
}
