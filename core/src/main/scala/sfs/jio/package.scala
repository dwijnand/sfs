package sfs

import java.nio.ByteBuffer
import java.nio.{ file => jnf }
import java.nio.{ channels => jnc }
import java.{ util => ju }
import jnf.{ attribute => jnfa }
import jnf.Files
import jnfa.PosixFilePermission._
import javax.naming.SizeLimitExceededException
import scala.collection.convert.{ AsJavaExtensions, AsScalaExtensions, StreamExtensions }
import api._

package object jio extends AsJavaExtensions with AsScalaExtensions with StreamExtensions with Alias {
  val UTF8          = java.nio.charset.StandardCharsets.UTF_8
  val UnixUserClass = Class.forName("sun.nio.fs.UnixUserPrincipals$User")
  val UidMethod     = UnixUserClass.getDeclaredMethod("uid").tap(_.setAccessible(true))

  def tmpDir(prefix: String): Path       = Files.createTempDirectory(prefix)
  def file(s: String, ss: String*): File = ss.foldLeft(new File(s))(new File(_, _))
  def homeDir: Path                      = path(sys.props("user.home"))
  def jList[A](xs: A*): jList[A]         = new ju.ArrayList[A].tap(x => xs.foreach(x.add))
  def jSet[A](xs: A*): jSet[A]           = new ju.HashSet[A].tap(x => xs.foreach(x.add))
  def path: String => Path               = jnf.Paths.get(_)

  implicit def pathOps(path: Path): PathOps = new PathOps(path)

  implicit class ClassOps[A](val c: Class[A]) extends AnyVal {
    def shortName: String = c.getName.stripSuffix("$").split("[.]").last.split("[$]").last
  }

  implicit class FilePathish(val f: File) extends AnyVal with Pathish[File] {
    def path: Path     = f.toPath
    def asRep(p: Path) = p.toFile
    def appending[A](g: FileOutputStream => A): A = Using.resource(new FileOutputStream(f, true))(g)
  }

  implicit class PathPathish(val path: Path) extends AnyVal with Pathish[Path] {
    def asRep(p: Path)      = p
    def append(other: Path) = jio.path(path.to_s + other.to_s)

    def permissions: PosixFilePermissions = {
      val pfp = path.nofollow.getPosixFilePermissions().asScala
      PosixFilePermissions(
        pfp(GROUP_READ) , pfp(GROUP_WRITE) , pfp(GROUP_EXECUTE),
        pfp(OWNER_READ) , pfp(OWNER_WRITE) , pfp(OWNER_EXECUTE),
        pfp(OTHERS_READ), pfp(OTHERS_WRITE), pfp(OTHERS_EXECUTE)
      )
    }

    def tryLock(): jnc.FileLock    = withWriteChannel(_.tryLock)

    def truncate(size: Long): Unit =
      withWriteChannel {
        case c if c.size > size => c.truncate(size)
        case c if c.size < size => c.appendNullBytes(at = c.size, amount = (size - c.size).toInt)
                                   if (c.size < size) throw new SizeLimitExceededException
        case _                  => // sizes are equal
      }

    private def withWriteChannel[A](code: FileChannel => A): A =
      Using.resource(jnc.FileChannel.open(path, jnf.StandardOpenOption.WRITE))(code)
  }

  implicit class FileChannelOps(val c: FileChannel) extends AnyVal {
    def appendNullBytes(at: Long, amount: Int): Unit =
      c.write(ByteBuffer.wrap(Array.fill(amount)(0.toByte)), at)
  }

  implicit class UnixPermsOps(val perms: UnixPerms) extends AnyVal {
    def java: Set[PosixFilePermission] = perms.bits.map(UnixToJava)
  }

  def toUnixMask(perms: jFilePermissions): Long = perms.asScala.map(JavaToUnix).foldLeft(0L)(_ | _)

  lazy val UnixToJava = UnixPerms.Bits.zip(JavaBits).toMap
  lazy val JavaToUnix = JavaBits.zip(UnixPerms.Bits).toMap

  lazy val JavaBits = Vector[PosixFilePermission](
    OWNER_READ, OWNER_WRITE, OWNER_EXECUTE,
    GROUP_READ, GROUP_WRITE, GROUP_EXECUTE,
    OTHERS_READ, OTHERS_WRITE, OTHERS_EXECUTE,
  )

  def bitsAsPermissions(bits: Long): jFilePermissions = {
    Set(
      (1 << 8, OWNER_READ), (1 << 7, OWNER_WRITE), (1 << 6, OWNER_EXECUTE),
      (1 << 5, GROUP_READ), (1 << 4, GROUP_WRITE), (1 << 3, GROUP_EXECUTE),
      (1 << 2, OTHERS_READ), (1 << 1, OTHERS_WRITE), (1 << 0, OTHERS_EXECUTE),
    ).foldLeft(Set.empty[PosixFilePermission]) { case (result, (bit, permission)) =>
      if ((bits & bit) == 0) result else result + permission
    }.asJava
  }
}
