package org.plasmalabs.models.protocol

import com.fasterxml.jackson.core.{JsonGenerator, JsonParser}
import com.fasterxml.jackson.databind.*
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.module.scala.*
import org.plasmalabs.models.utility.Ratio
import org.plasmalabs.quivr.models
import org.plasmalabs.quivr.models.Int128
import org.plasmalabs.sdk.models.box.Value.*
import org.plasmalabs.sdk.syntax.*

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.Try

object ConfigConverter {
  private val module: SimpleModule = new SimpleModule("RatioSerializerDeserializer")
  module.addSerializer(classOf[models.Ratio], new RatioCodec.RatioSerializer())
  module.addDeserializer(classOf[models.Ratio], new RatioCodec.RatioDeserializer())

  private val jsonMapper = (JsonMapper
    .builder()
    .addModule(DefaultScalaModule)
    .addModule(module)
    .enable(DeserializationFeature.FAIL_ON_NULL_CREATOR_PROPERTIES)
    .enable(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY)
    .build() :: ClassTagExtensions)

  val jsonConfigKey: String = "jsonConfig"

  def extract[T: ClassTag](config: ConfigProposal): Either[Throwable, T] =
    config.value.get(jsonConfigKey).toRight(new IllegalArgumentException(s"Key $jsonConfigKey is missed")).flatMap {
      config =>
        val res = Try(jsonMapper.readValue[T](config))
        res.toEither
    }

  def extractOrRaise[T: ClassTag](config: ConfigProposal): T =
    extract(config).toOption.get

  def pack[T](config: T): ConfigProposal = {
    val configString = jsonMapper.writeValueAsString(config)
    val proposalMap = Map(jsonConfigKey -> configString)
    ConfigProposal(proposalMap)
  }

}

object RatioCodec {
  private val numeratorField = "numerator"
  private val denominatorField = "denominator"

  implicit def ratioToProtoRatio(ratio: Ratio): models.Ratio =
    models.Ratio(ratio.numerator: Int128, ratio.denominator: Int128)

  class RatioDeserializer() extends JsonDeserializer[models.Ratio] {

    override def deserialize(p: JsonParser, context: DeserializationContext): models.Ratio = {
      val node: JsonNode = p.getCodec.readTree(p)
      val numerator: Int128 = BigInt(node.get(numeratorField).asText())
      val denominator: Int128 = BigInt(node.get(denominatorField).asText())
      models.Ratio(numerator, denominator)
    }
  }

  class RatioSerializer() extends JsonSerializer[models.Ratio] {

    override def serialize(value: models.Ratio, gen: JsonGenerator, serializers: SerializerProvider): Unit = {
      val numerator = BigInt(value.numerator.value.toByteArray).toString()
      val denominator = BigInt(value.denominator.value.toByteArray).toString()
      gen.writeStartObject()
      gen.writeFieldName(numeratorField)
      gen.writeString(numerator)
      gen.writeFieldName(denominatorField)
      gen.writeString(denominator)
      gen.writeEndObject()
    }
  }

}
