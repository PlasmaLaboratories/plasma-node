package org.plasmalabs.codecs.bytes

/**
 * This package contains the codecs used to serialize/deserialize node object to/from BitVector. BitVectors are
 * easily converted to/from Array[Byte]
 */
package object tetra {

  object instances
      extends ProtoIdentifiableOps
      with TetraPersistableCodecs
      with TetraSignableCodecs
      with TetraTransmittableCodecs
}
