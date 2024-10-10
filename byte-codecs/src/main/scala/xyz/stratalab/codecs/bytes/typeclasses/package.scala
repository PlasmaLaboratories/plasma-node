package xyz.stratalab.codecs.bytes

package object typeclasses {

  trait Implicits
      extends Persistable.ToPersistableOps
      with Persistable.ToExtensionOps
      with Transmittable.ToTransmittableOps
      with Transmittable.ToExtensionOps
      with Identifiable.ToIdentifiableOps
      with Signable.ToSignableOps

  object implicits extends Implicits
}
