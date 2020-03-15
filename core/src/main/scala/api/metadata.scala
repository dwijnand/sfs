package sfs
package api

/*
 * Conventions:
 *
 * - methods that are intended to be used with 'type parameters only' should be written:
 *     `def name[A]()(implicit z: Implicit[A]): ReturnType`
 *   The argument list should be empty and the implicit should be in the second argument list.
 *   This makes explicit passing of the argument awkward. Implicit arguments are typically called `z`
 * - limit the use of `val` to an absolute minimum
 * - think twice before writing the `extends` or `override` keyword.
 */

/** Key is an attribute key: a Metadata map key which carries
 *  both the type of the corresponding value, and a value to
 *  provide when there is no value in the map. This as opposed
 *  to wrapping everything in sight in Option. */
final class Key[A](description: String) extends ShowSelf { def to_s = description }

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

/** A Metadata map, holding any number of well typed Attributes. A typed value can be obtained for any key. */
final class Metadata(val attributes: Vector[Attribute]) extends ShowSelf { md =>
  private val untypedMap                         = attributes.foldLeft(Map[Key[_], Any]())(_ + _.pair)
  private def untypedAs[A]()(implicit z: Key[A]) = untypedMap(z).asInstanceOf[A]

  def transform(f: Vector[Attribute] => Vector[Attribute]): Metadata = new Metadata(f(attributes))

  def isEmpty                                  = attributes.isEmpty
  def keys: Vector[Key[_]]                     = attributes.map(_.key)
  def has[A]()(implicit z: Key[A]): Boolean    = attributes.exists(_.hasKey(z))

  def drop[A]()(implicit z: Key[A]): Metadata  = if (has[A]) transform(_.filterNot(_.hasKey(z))) else this
  def drop(attr: Attribute): Metadata          = drop()(attr.key)
  def set(attr: Attribute): Metadata           = drop(attr).transform(_ :+ attr) // or preserve history
  def set[A: Key](value: A): Metadata          = set(Attribute[A](value))

  def fold[A]: Fold[A] = new Fold[A]

  class Fold[A] {
    def apply[B](f: A => B, g: => B)(implicit z: Key[A]): B = if (has[A]) f(untypedAs[A]) else g
  }

  def only[A: Key : Empty] = new Only[A]

  class Only[A: Key : Empty] {
    def map(f: A => A): Metadata             = md.set(f(apply[A]))
    def flatMap(f: A => Option[A]): Metadata = f(md[A]()).fold(md)(md.set(_))
    def filter(p: A => Boolean): Metadata    = if (has[A] && !p(md[A])) md.drop[A] else md
  }

  def apply[A: Empty]()(implicit z: Key[A]): A = fold[A](idFun[A], empty[A])

  def foreach(f: Any =?> Unit): Unit = attributes.foreach(a => f.applyOrElse(a.value, void))

  def map(f: Attribute => Attribute): Metadata       = transform(_.map(f))
  def flatMap(f: Attribute => Metadata): Metadata    = transform(_.flatMap(f(_).attributes))
  def filter(p: Attribute => Boolean): Metadata      = transform(_.filter(p))

  def mapOnly(pf: Attribute =?> Attribute): Metadata = map(x => pf.applyOrElse(x, constV(x)))

  def to_s = if (isEmpty) "{ }" else attributes.mkString("{\n  ", "\n  ", "\n}")
}

object Metadata {
  def apply(attributes: Attribute*): Metadata = new Metadata(attributes.toVector)
}
