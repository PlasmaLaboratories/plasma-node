package org.plasmalabs.codecs.bytes.tetra

import org.plasmalabs.codecs.bytes.typeclasses.*
import org.plasmalabs.models.UnsignedBlockHeader

trait TetraSignableCodecs {

  implicit val signableUnsignedConsensusBlockHeader: Signable[UnsignedBlockHeader] =
    Signable.fromScodecEncoder(using TetraScodecCodecs.unsignedBlockHeaderCodec)

}

object TetraSignableCodecs extends TetraSignableCodecs
