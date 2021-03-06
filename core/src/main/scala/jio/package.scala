package sfs

import java.nio.ByteBuffer
import java.nio.{ file => jnf }
import java.nio.{ channels => jnc }
import jnf.{ attribute => jnfa }
import jnf.{ Files }
import jnf.LinkOption.NOFOLLOW_LINKS
import java.util.concurrent.TimeUnit
import javax.naming.SizeLimitExceededException
import scala.collection.convert.{ DecorateAsScala, DecorateAsJava }
import api._, attributes.Mtime

package object jio extends DecorateAsScala with DecorateAsJava with Alias {
  val UTF8          = java.nio.charset.Charset forName "UTF-8"
  val UnixUserClass = Class.forName("sun.nio.fs.UnixUserPrincipals$User")
  val UidMethod     = doto(UnixUserClass getDeclaredMethod "uid")(_ setAccessible true)

  def createTempDirectory(prefix: String): Path = Files.createTempDirectory(prefix)
  def file(s: String, ss: String*): File        = ss.foldLeft(new File(s))(new File(_, _))
  def homeDir: Path                             = path(sys.props("user.home"))
  def jList[A](xs: A*): jList[A]                = doto(new java.util.ArrayList[A])(xs foreach _.add)
  def jSet[A](xs: A*): jSet[A]                  = doto(new java.util.HashSet[A])(xs foreach _.add)
  def path: String => Path                      = jnf.Paths get _

  implicit class JioFilesInstanceOps(path: Path) extends JioFilesInstance(path)

  implicit class StreamOps[A](val xs: jStream[A]) extends AnyVal {
    def toVector: Vector[A] = {
      val buf = Vector.newBuilder[A]
      xs.iterator.asScala foreach (x => buf += x)
      buf.result
    }
  }

  implicit class ClassOps[A](val c: Class[A]) {
    def shortName: String = (c.getName.stripSuffix("$") split "[.]").last split "[$]" last
  }

  implicit class FileOps(val f: File) extends Pathish[File] {
    def path: Path     = f.toPath
    def asRep(p: Path) = p.toFile

    def appending[A](g: FileOutputStream => A): A = {
      val stream = new FileOutputStream(f, true) // append = true
      try g(stream) finally stream.close()
    }
  }
  implicit class PathOps(val path: Path) extends Pathish[Path] {
    def asRep(p: Path) = p

    def append(other: Path) = jio.path(path.to_s + other.to_s)

    def permissions: PosixFilePermissions = {
      val pfp = (Files getPosixFilePermissions (path, NOFOLLOW_LINKS)).asScala
      import jnfa.PosixFilePermission._
      PosixFilePermissions(
        pfp(GROUP_READ) , pfp(GROUP_WRITE) , pfp(GROUP_EXECUTE),
        pfp(OWNER_READ) , pfp(OWNER_WRITE) , pfp(OWNER_EXECUTE),
        pfp(OTHERS_READ), pfp(OTHERS_WRITE), pfp(OTHERS_EXECUTE)
      )
    }

    def tryLock():jnc.FileLock     = withWriteChannel(_.tryLock)
    def truncate(size: Long): Unit = withWriteChannel {
      case c if c.size > size => c truncate size
      case c if c.size < size => c appendNullBytes (at = c.size, amount = (size - c.size).toInt)
                                 if (c.size < size) throw new SizeLimitExceededException
      case _                  => // sizes are equal
    }

    private def withWriteChannel[A](code: jnc.FileChannel => A): A = {
      val channel = jnc.FileChannel.open(path, jnf.StandardOpenOption.WRITE)
      try code(channel)
      finally channel.close()
    }
  }

  implicit class FileChannelOps(val c: FileChannel) extends AnyVal {
    def appendNullBytes(at: Long, amount: Int): Unit = {
      val nullBytes = Array.fill(amount)(0.toByte)
      c write (ByteBuffer wrap nullBytes, at)
    }
  }
  case class PosixFilePermissions(
    groupRead: Boolean, groupWrite: Boolean, groupExecute: Boolean,
    ownerRead: Boolean, ownerWrite: Boolean, ownerExecute: Boolean,
    otherRead: Boolean, otherWrite: Boolean, otherExecute: Boolean
  )
  implicit class UnixPermsOps(val perms: api.attributes.UnixPerms) extends AnyVal {
    def java: Set[PosixFilePermission] = perms.bits map UnixToJava
  }

  def toUnixMask(perms: jFilePermissions) =
    ( perms.asScala map JavaToUnix ).foldLeft(0L)(_ | _)

  lazy val UnixToJava = (api.attributes.UnixPerms.Bits zip JavaBits).toMap
  lazy val JavaToUnix = (JavaBits zip api.attributes.UnixPerms.Bits).toMap

  lazy val JavaBits = {
    import jnfa.PosixFilePermission._
    Vector[PosixFilePermission](
      OWNER_READ,
      OWNER_WRITE,
      OWNER_EXECUTE,
      GROUP_READ,
      GROUP_WRITE,
      GROUP_EXECUTE,
      OTHERS_READ,
      OTHERS_WRITE,
      OTHERS_EXECUTE
    )
  }

  def bitsAsPermissions(bits: Long): jFilePermissions = {
    import jnfa.PosixFilePermission._
    val permissionBits = Set(
      (1 << 8, OWNER_READ),
      (1 << 7, OWNER_WRITE),
      (1 << 6, OWNER_EXECUTE),
      (1 << 5, GROUP_READ),
      (1 << 4, GROUP_WRITE),
      (1 << 3, GROUP_EXECUTE),
      (1 << 2, OTHERS_READ),
      (1 << 1, OTHERS_WRITE),
      (1 << 0, OTHERS_EXECUTE)
    )
    val permissions =
      permissionBits.foldLeft(Set.empty[PosixFilePermission]) {
        case (result, (bit, permission)) =>
          if ((bits & bit) == 0) result
          else result + permission
      }
    permissions.asJava
  }
}
