package org.plasmalabs.codecs.bytes.scodecs

package object ops {
  trait Implicits extends ScodecOps.ToScodecOps
  object implicits extends Implicits
}
