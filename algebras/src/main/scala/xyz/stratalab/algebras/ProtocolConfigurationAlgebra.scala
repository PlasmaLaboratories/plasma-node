package xyz.stratalab.algebras

import xyz.stratalab.proto.node.NodeConfig

/**
 * Configuration Rpc
 * An interaction layer intended for users/clients of a blockchain node.
 *
 * @tparam F Effect type
 * @tparam S Node protocol configurations changes container, Ex: Stream, Seq
 */
trait ProtocolConfigurationAlgebra[F[_], S[_]] {

  def fetchNodeConfig: F[S[NodeConfig]]
}
