package co.topl.http.api.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.http.api.ApiRouteWithView
import co.topl.modifier.ModifierId
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.settings.{AppContext, RESTApiSettings}
import io.circe.Json
import io.circe.syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class NodeViewApiRoute(override val settings: RESTApiSettings, appContext: AppContext, nodeViewHolderRef: ActorRef)
                           (implicit val context: ActorRefFactory) extends ApiRouteWithView {
  type HIS = History
  type MS = State
  type MP = MemPool
  override val route: Route ={ basicRoute(handlers) }

  // Establish the expected network prefix for addresses
  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  def handlers(method: String, params: Vector[Json], id: String): Future[Json] =
    method match {
      case "mempool"                => mempool(params.head, id)
      case "transactionById"        => transactionById(params.head, id)
      case "blockById"              => blockById(params.head, id)
      case "transactionFromMempool" => transactionFromMempool(params.head, id)
    }

  /**  #### Summary
    *    Get the first 100 transactions in the mempool (sorted by fee amount)
    *
    * ---
    *  #### Params
    * 
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                 |
    *  |-------------------------	|-----------	|---------------------	|--------------------------------------------	|
    *  | --None specified--       |           	|                     	|                                             |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def mempool(params: Json, id: String): Future[Json] = {
    viewAsync().map {
      view => view.pool.take(100).asJson
    }
  }

  /**  #### Summary
    *    Lookup a transaction by its id
    *
    * ---
    *  #### Params
    * 
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                	|
    *  |-------------------------	|-----------	|---------------------	|-------------------------------------------	|
    *  | transactionId            | String    	| Required            	| Base58 encoded transaction hash             |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def transactionById(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      // parse required arguments
      val transactionId: ModifierId = ModifierId((params \\ "transactionId").head.asString.get)

      val blockId = view.history.blockContainingTx(transactionId).get
      val blockNumber = view.history.storage.heightOf(blockId)
      val tx = view.history.storage
        .modifierById(blockId)
        .get
        .transactions
        .filter(_.id == transactionId)
        .head

      tx.asJson.deepMerge{
        Map("blockNumber" -> blockNumber.toString,
          "blockId" -> blockId.toString
        ).asJson
      }
    }
  }

  /**  #### Summary
    *    Lookup a transaction in the mempool by its id
    *
    * ---
    *  #### Params
    * 
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                               	|
    *  |-------------------------	|-----------	|---------------------	|--------------------------------------------	|
    *  | transactionId            | String    	| Required            	| Base58 encoded transaction hash             |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def transactionFromMempool(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val transactionId: ModifierId = ModifierId((params \\ "transactionId").head.asString.get)
      view.pool.modifierById(transactionId) match {
        case Some(tx) => tx.asJson
        case None     => throw new Error("Unable to retrieve transaction")
      }
    }
  }

  /**  #### Summary
    *   Lookup a block by its id
    *
    * ---
    *  #### Params
    * 
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                	|
    *  |-------------------------	|-----------	|---------------------	|--------------------------------------------	|
    *  | blockId                  | String    	| Required            	| Base58 encoded transaction hash             |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def blockById(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val blockId: ModifierId = ModifierId((params \\ "blockId").head.asString.get)
      view.history
        .modifierById(blockId)
        .get
        .asJson
        .asObject
        .get
        .add("blockNumber", view.history.storage.heightOf(blockId).asJson)
        .add("blockDifficulty", view.history.storage.difficultyOf(blockId).asJson)
        .asJson
    }
  }
}