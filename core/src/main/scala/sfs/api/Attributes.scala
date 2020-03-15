package sfs
package api

object Attributes {
  // underscore on implicits to prevent shadowing
  implicit val _unixPerms  = new Key[UnixPerms]("unix permissions")
  implicit val _nodeType   = new Key[NodeType]("type of node")
  implicit val _mtime      = new Key[Mtime]("modification time in ...")
  implicit val _atime      = new Key[Atime]("access time in ...")
  implicit val _size       = new Key[Size]("size in bytes")
  implicit val _uid        = new Key[Uid]("uid ...")
  implicit val _blockCount = new Key[BlockCount]("number of blocks")
}
