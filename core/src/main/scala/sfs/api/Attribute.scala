package sfs
package api

/** Attribute is a dependently typed key/value pair. */
trait Attribute extends Any with ShowDirect {
  type Type
  def key: Key[Type]
  def value: Type

  def hasSameKey(that: Attribute)      = hasKey(that.key)
  def hasKey[A](implicit that: Key[A]) = key == that

  def pair = key -> value
  def to_s = s"$key: $value"
}

object Attribute {
  type Of[A] = Attribute { type Type = A }

  implicit def apply[A](value: A)(implicit key: Key[A]): Of[A] = AttributeOf[A](key, value)
  def unapply(x: Attribute): Some[x.Type]                      = Some(x.value)

  /** The simple implementation of Attribute, which ties the knot between the
   *  user-facing type parameter and the type member which couples key and value. */
  private case class AttributeOf[A](key: Key[A], value: A) extends Attribute with ShowSelf { type Type = A }
}
