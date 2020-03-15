package sfs
package api

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
