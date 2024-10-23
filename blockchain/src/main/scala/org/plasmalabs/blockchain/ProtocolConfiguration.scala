package org.plasmalabs.blockchain

import cats.effect.{Async, Resource}
import fs2.Stream
import org.plasmalabs.algebras.ProtocolConfigurationAlgebra
import org.plasmalabs.proto.node.NodeConfig

/**
 * Emits a stream of node protocol configs.
 */
object ProtocolConfiguration {

  def make[F[_]: Async](
    nodeProtocolConfigs: Seq[NodeConfig]
  ): Resource[F, ProtocolConfigurationAlgebra[F, Stream[F, *]]] =
    Resource.pure(
      new ProtocolConfigurationAlgebra[F, Stream[F, *]] {

        override def fetchNodeConfig: F[Stream[F, NodeConfig]] =
          Async[F].delay(
            Stream.emits[F, NodeConfig](nodeProtocolConfigs)
          )
      }
    )
}
