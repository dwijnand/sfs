package sfs
package api

import java.nio.file.attribute.PosixFilePermission._
import std._, jio._

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


  implicit class Ops(val perms: UnixPerms) extends AnyVal {
    def java: Set[PosixFilePermission] = perms.bits.map(UnixToJava)
  }

  def fromJava(perms: jFilePermissions): UnixPerms =
    UnixPerms(perms.asScala.map(JavaToUnix).foldLeft(0L)(_ | _))

  lazy val UnixToJava = UnixPerms.Bits.zip(JavaBits).toMap
  lazy val JavaToUnix = JavaBits.zip(UnixPerms.Bits).toMap

  lazy val JavaBits = Vector[PosixFilePermission](
    OWNER_READ, OWNER_WRITE, OWNER_EXECUTE,
    GROUP_READ, GROUP_WRITE, GROUP_EXECUTE,
    OTHERS_READ, OTHERS_WRITE, OTHERS_EXECUTE,
  )
}
