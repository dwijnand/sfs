package sfs
package api

import std._

/** Key is an attribute key: a Metadata map key which carries
 *  both the type of the corresponding value, and a value to
 *  provide when there is no value in the map. This as opposed
 *  to wrapping everything in sight in Option. */
final class Key[A](description: String) extends ShowSelf { def to_s = description }

