package xyz.stratalab.genus.orientDb.instances

import xyz.stratalab.sdk.codecs.AddressCodecs
import co.topl.brambl.models.{LockAddress, LockId}
import xyz.stratalab.genus.orientDb.schema.OTyped.Instances._
import xyz.stratalab.genus.orientDb.schema.{GraphDataEncoder, OIndexable, VertexSchema}

object SchemaLockAddress {

  /**
   * Address model:
   * @see https://github.com/Topl/protobuf-specs/blob/main/proto/brambl/models/address.proto
   */
  object Field {
    val SchemaName = "LockAddress"
    val Network = "network"
    val Ledger = "ledger"
    // id on proto models, do not use id, Property key is reserved for all elements: id
    val AddressId = "addressId"
    val AddressEncodedId = "addressEncodedId"
    val AddressIndex = "addressIndex"
  }

  def make(): VertexSchema[LockAddress] =
    VertexSchema.create(
      Field.SchemaName,
      GraphDataEncoder[LockAddress]
        .withProperty(
          Field.Network,
          lockAddress => java.lang.Integer.valueOf(lockAddress.network),
          mandatory = true,
          readOnly = true,
          notNull = true
        )
        .withProperty(
          Field.Ledger,
          lockAddress => java.lang.Integer.valueOf(lockAddress.ledger),
          mandatory = true,
          readOnly = true,
          notNull = true
        )
        .withProperty(
          Field.AddressId,
          _.id.value.toByteArray,
          mandatory = true,
          readOnly = true,
          notNull = true
        )
        .withProperty(
          Field.AddressEncodedId,
          lockAddress => AddressCodecs.encodeAddress(lockAddress),
          mandatory = true,
          readOnly = true,
          notNull = true
        )
        .withIndex[LockAddress](Field.AddressIndex, Field.AddressId)(using OIndexable.Instances.address),
      v =>
        LockAddress(
          network = v(Field.Network),
          ledger = v(Field.Ledger),
          id = LockId.parseFrom(v(Field.AddressId))
        )
    )
}
