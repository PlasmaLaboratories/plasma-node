package xyz.stratalab.codecs.bytes.tetra

import xyz.stratalab.codecs.bytes.typeclasses._
import xyz.stratalab.models.UnsignedBlockHeader

trait TetraSignableCodecs {

  implicit val signableUnsignedConsensusBlockHeader: Signable[UnsignedBlockHeader] =
    Signable.fromScodecEncoder(TetraScodecCodecs.unsignedBlockHeaderCodec)

}

object TetraSignableCodecs extends TetraSignableCodecs
