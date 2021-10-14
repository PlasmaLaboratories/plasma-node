package co.topl.tools.exporter

import org.mongodb.scala.model.Filters.{and, gte, lte}
import org.mongodb.scala.model.{Projections, ReplaceOneModel, ReplaceOptions, WriteModel}
import org.mongodb.scala.{BulkWriteResult, Document, MongoClient}

import scala.concurrent.Future

class MongoChainRepExport(uri: String, database: String) {

  private val client = open(uri)
  private val db = client.getDatabase(database)

  private def open(uri: String): MongoClient = MongoClient(uri)

  def checkValidConnection(): Future[Seq[String]] = db.listCollectionNames().toFuture()

  def replaceInsert(eleSeq: Seq[(String, String)], filterField: String, dt: DataType): Future[BulkWriteResult] = {
    val updateOpt: ReplaceOptions = ReplaceOptions().upsert(true)
    val writes: List[WriteModel[_ <: Document]] =
      eleSeq.map { case (id, modifierString) =>
        ReplaceOneModel(Document(filterField -> id), Document(modifierString), updateOpt)
      }.toList
    db.getCollection(dt.name).bulkWrite(writes).toFuture()
  }

  def getExistingHeights(start: Long, end: Long): Future[Seq[Long]] = db
    .getCollection(DataType.Block.name)
    .find(and(gte("height", start), lte("height", end)))
    .projection(Projections.fields(Projections.include("height"), Projections.excludeId()))
    .map(_.head._2.asNumber().longValue())
    .toFuture()

  def close(): Unit = client.close()
}

object MongoChainRepExport {

  def apply(uri: String, db: String): MongoChainRepExport =
    new MongoChainRepExport(uri, db)
}
