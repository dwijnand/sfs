package sfs
package api

object attributes {
  // underscore on implicits to prevent shadowing
  implicit val _unixPerms  = new Key[UnixPerms]("unix permissions")
  implicit val _nodeType   = new Key[NodeType]("type of node")
  implicit val _mtime      = new Key[Mtime]("modification time in ...")
  implicit val _atime      = new Key[Atime]("access time in ...")
  implicit val _size       = new Key[Size]("size in bytes")
  implicit val _uid        = new Key[Uid]("uid ...")
  implicit val _blockCount = new Key[BlockCount]("number of blocks")

  val File = new NodeType("file")
  val Dir  = new NodeType("dir" )
  val Link = new NodeType("link")

  final case class UnixPerms(mask: Long) extends ShowSelf {
    def bits: Set[Long] = UnixPerms.BitsSet.filter(bit => (bit & mask) != 0)
    def to_s            = UnixPerms.permString(mask)
  }

  object UnixPerms {
    val Bits = Vector[Long](1 << 8, 1 << 7, 1 << 6, 1 << 5, 1 << 4, 1 << 3, 1 << 2, 1 << 1, 1 << 0)
    private val BitsSet: Set[Long]    = Bits.toSet
    private val Letters: Vector[Char] = "rwxrwxrwx".toVector

    private def permString(mask: Long): String =
      (for ((perm, ch) <- Bits.zip(Letters)) yield if ((mask & perm) == 0) '-' else ch).mkString("")
  }

  final class NodeType(`type`: String) extends ShowSelf { def to_s = `type` }

  abstract class FileTimeBased[This](create: FileTime => This) {
    def timestamp: FileTime
    def +(amount: Duration): This = create(timestamp + amount)
  }

  final case class Mtime(timestamp: FileTime) extends FileTimeBased[Mtime](Mtime(_)) {
    override def hashCode = timestamp.toMillis.##
    override def equals(that: Any) = that match {
      case Mtime(other) => timestamp.toMillis == other.toMillis
      case _            => super.equals(that)
    }
  }

  final case class Atime(timestamp: FileTime)
  final case class Size(bytes: Long)
  final case class Uid(value: Int)
  final case class BlockCount(amount: Long)
}
