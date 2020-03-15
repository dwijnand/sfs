package sfs
package api

final case class Mtime(timestamp: FileTime) extends FileTimeBased[Mtime](Mtime(_))
