package sfs
package fuse

import std._

abstract class FuseFsFull extends FuseFs {
  final def logging(): this.type = doto(this)(_.log(true)) // can't be trait
}