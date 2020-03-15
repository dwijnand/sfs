package sfs
package api

import std._

final case class Mtime(timestamp: FileTime) extends FileTimeBased[Mtime](Mtime(_))
