package sfs
package api

import std._

final case class Atime(timestamp: FileTime) extends FileTimeBased[Atime](Atime(_))
