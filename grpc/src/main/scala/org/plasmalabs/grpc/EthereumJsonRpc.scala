package xyz.stratalab.grpc

import cats.data.OptionT
import cats.effect.{Async, Resource}
import cats.implicits._
import com.comcast.ip4s.{Host, Port}
import fs2.io.net.Network
import io.circe._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.circe._
import org.http4s.dsl.Http4sDslBinCompat
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.http4s.server.middleware.CORS
import org.typelevel.log4cats.Logger
import scodec.bits.ByteVector
import xyz.stratalab.blockchain.{BigBang, BlockchainCore}
import xyz.stratalab.typeclasses.implicits.showBlockId

object EthereumJsonRpc {

  def serve[F[_]: Async: Network: Logger](bindHost: String, bindPort: Int)(
    methods: EthereumRpcMethods[F]
  ): Resource[F, Unit] =
    Resource
      .eval(
        Async[F]
          .delay(
            (Host.fromString(bindHost), Port.fromInt(bindPort)).tupled
              .toRight(new IllegalArgumentException("Invalid bindHost/bindPort"))
          )
          .rethrow
      )
      .flatMap { case (host, port) =>
        EmberServerBuilder
          .default[F]
          .withHost(host)
          .withPort(port)
          .withHttpApp(
            CORS.policy.withAllowOriginAll.withAllowMethodsAll
              .withAllowHeadersAll(
                Router("/" -> routes[F](methods))
              )
              .orNotFound
          )
          .build
          .evalTap(_ => Logger[F].info(s"JSON RPC server started on $host:$port"))
      }
      .void

  def routes[F[_]: Async: Logger](methods: EthereumRpcMethods[F]): HttpRoutes[F] = {
    val dsl = new Http4sDslBinCompat[F] {}
    import JsonRpcRequest._
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root => Ok()
      case req @ POST -> Root =>
        req
          .as[JsonRpcRequest]
          .flatMap(request =>
            (request.method match {
              case "eth_blockNumber" =>
                methods.blockNumber.map(number => request.successResponse(s"0x${number.toHexString}".asJson))
              case "eth_chainId" =>
                methods.chainId.map(number => request.successResponse(s"0x${number.toHexString}".asJson))
              case "eth_getBlockByNumber" =>
                (
                  Async[F].fromEither(request.params.head.as[String]),
                  Async[F].fromEither(request.params.lift(1).fold[Decoder.Result[Boolean]](Right(false))(_.as[Boolean]))
                ).tupled
                  .flatMap { case (num, hydrate) => methods.blockByNumber(num, hydrate) }
                  .map(_.getOrElse(Json.Null))
                  .map(request.successResponse)
              case "eth_getBalance" =>
                (
                  Async[F].fromEither(request.params.head.as[String]),
                  Async[F].fromEither(request.params.lift(1).traverse(_.as[String]))
                ).tupled
                  .flatMap { case (address, block) => methods.getBalance(address, block) }
                  .map(number => request.successResponse(number.asJson))
              case "net_version" =>
                methods.netVersion
                  .map(_.asJson)
                  .map(request.successResponse)
              case _ =>
                request.errorResponse(-32601, "Method not found").pure[F]
            })
              .recoverWith { case e =>
                Logger[F].warn(e)("Ethereum JSON RPC Error").as(request.errorResponse(-32603, "Borked"))
              }
              .map(_.asJson)
              .map(Response().withEntity(_))
          )
    }
  }

}

case class JsonRpcRequest(jsonrpc: String = "2.0", method: String, params: List[Json], id: Option[Json]) {
  def successResponse(result: Json): JsonRpcResponse = JsonRpcSuccess(jsonrpc, result, id)

  def errorResponse(code: Int, message: String, data: Option[Json] = None): JsonRpcResponse =
    JsonRpcError(jsonrpc, code, message, data, id)
}

object JsonRpcRequest {

  implicit val jsonRpcRequestDecoder: Decoder[JsonRpcRequest] =
    c =>
      for {
        jsonRpc <- c.get[String]("jsonrpc")
        method  <- c.get[String]("method")
        params  <- c.get[List[Json]]("params")
        id      <- c.get[Option[Json]]("id")
      } yield JsonRpcRequest(jsonRpc, method, params, id)

  implicit val jsonRpcRequestEncoder: Encoder[JsonRpcRequest] =
    r =>
      Json.obj(
        "jsonrpc" -> r.jsonrpc.asJson,
        "method"  -> r.method.asJson,
        "params"  -> r.params.asJson,
        "id"      -> r.id.asJson
      )
}

sealed abstract class JsonRpcResponse
case class JsonRpcSuccess(jsonrpc: String = "2.0", result: Json, id: Option[Json]) extends JsonRpcResponse

case class JsonRpcError(jsonrpc: String = "2.0", code: Int, message: String, data: Option[Json], id: Option[Json])
    extends JsonRpcResponse

object JsonRpcResponse {

  implicit val jsonRpcResponseEncoder: Encoder[JsonRpcResponse] = {
    case JsonRpcSuccess(jsonrpc, result, id) =>
      Json.obj(
        "jsonrpc" -> jsonrpc.asJson,
        "result"  -> result,
        "id"      -> id.asJson
      )
    case JsonRpcError(jsonrpc, code, message, data, id) =>
      Json.obj(
        "jsonrpc" -> jsonrpc.asJson,
        "error"   -> Json.obj("code" -> code.asJson, "message" -> message.asJson, "data" -> data.asJson),
        "id"      -> id.asJson
      )
  }
}

trait EthereumRpcMethods[F[_]] {
  def blockNumber: F[Long]
  def chainId: F[Long]
  def netVersion: F[String]
  def getBalance(address:   String, block:                Option[String]): F[String]
  def blockByNumber(number: String, hydratedTransactions: Boolean): F[Option[Json]]
}

class EthereumJsonRpcImpl[F[_]: Async](core: BlockchainCore[F]) extends EthereumRpcMethods[F] {

  override def blockNumber: F[Long] = core.consensus.localChain.head.map(_.height)

  override def chainId: F[Long] = 69420L.pure[F]

  override def netVersion: F[String] = "69420".pure[F]

  override def blockByNumber(number: String, hydratedTransactions: Boolean): F[Option[Json]] =
    Async[F]
      .delay(number match {
        case "earliest"              => BigBang.Height
        case "finalized"             => -10L // TODO
        case "safe"                  => -5L
        case "latest"                => 0L
        case v if v.startsWith("0x") => java.lang.Long.parseLong(v.drop(2), 16)
        case v                       => v.toLong
      })
      .flatMap(height =>
        OptionT(core.consensus.localChain.blockIdAtHeight(height))
          .semiflatMap(id =>
            core.dataStores.headers
              .getOrRaise(id)
              .map(header =>
                Json.obj(
                  "hash"            -> s"0x${ByteVector(id.value.toByteArray).toHex}".asJson,
                  "parentHash"      -> s"0x${ByteVector(header.parentHeaderId.value.toByteArray).toHex}".asJson,
                  "sha3Uncles"      -> ("0x" + Array.fill(64)("0").mkString).asJson,
                  "miner"           -> ("0x" + Array.fill(64)("40").mkString).asJson,
                  "stateRoot"       -> ("0x" + Array.fill(64)("0").mkString).asJson,
                  "transactionRoot" -> ("0x" + Array.fill(64)("0").mkString).asJson,
                  "receiptsRoot"    -> ("0x" + Array.fill(64)("0").mkString).asJson,
                  "logsBloom"       -> ("0x" + Array.fill(512)("0").mkString).asJson,
                  "difficulty"      -> "0x0".asJson,
                  "number"          -> s"0x${header.height.toHexString}".asJson,
                  "gasLimit"        -> "0x0".asJson,
                  "gasUsed"         -> "0x0".asJson,
                  "timestamp"       -> s"0x${header.timestamp.toHexString}".asJson,
                  "extraData"       -> "0x0".asJson,
                  "mixHash"         -> ("0x" + Array.fill(64)("0").mkString).asJson,
                  "nonce"           -> ("0x" + Array.fill(16)("0").mkString).asJson,
                  "size"            -> s"0x${header.toByteArray.length.toHexString}".asJson,
                  "transactions"    -> Json.arr(),
                  "withdrawals"     -> Json.arr(),
                  "uncles"          -> Json.arr()
                )
              )
          )
          .value
      )

  override def getBalance(address: String, block: Option[String]): F[String] = "0xeb344079513a1300000".pure[F]
}
