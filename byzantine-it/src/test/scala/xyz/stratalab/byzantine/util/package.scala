package xyz.stratalab.byzantine

import xyz.stratalab.algebras.IndexerRpc
import com.spotify.docker.client.DockerClient
import scala.language.implicitConversions

package object util {

  implicit def rpcToIndexerRpcApi[F[_]](rpc: IndexerRpc[F]): IndexerRpcApi[F] =
    new IndexerRpcApi(rpc)

  implicit def nodeToDockerApi(node: DockerNode)(implicit
    dockerClient: DockerClient
  ): NodeDockerApi =
    new NodeDockerApi(node.containerId)
}
