package sfs
package api

final case class Atime(timestamp: FileTime) extends FileTimeBased[Atime](Atime(_))
