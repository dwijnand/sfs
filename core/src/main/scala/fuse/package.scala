package sfs

import java.nio.file._
import javax.naming.SizeLimitExceededException
import net.fusejna.ErrorCodes._
import api._, attributes._

package object fuse {
  type DirectoryFiller   = net.fusejna.DirectoryFiller
  type FileInfo          = net.fusejna.StructFuseFileInfo.FileInfoWrapper
  type FlockCommand      = net.fusejna.FlockCommand
  type FlockWrapper      = net.fusejna.StructFlock.FlockWrapper
  type FuseContext       = net.fusejna.StructFuseContext
  type FuseException     = net.fusejna.FuseException
  type FuseFilesystem    = net.fusejna.FuseFilesystem
  type IModeInfo         = net.fusejna.types.TypeMode.IModeWrapper
  type ModeInfo          = net.fusejna.types.TypeMode.ModeWrapper
  type OpenMode          = net.fusejna.StructFuseFileInfo.FileInfoWrapper.OpenMode
  type StatInfo          = net.fusejna.StructStat.StatWrapper
  type StatvfsWrapper    = net.fusejna.StructStatvfs.StatvfsWrapper
  type TimeBufferWrapper = net.fusejna.StructTimeBuffer.TimeBufferWrapper
  type Timespec          = net.fusejna.StructTimespec.ByValue
  type XattrFiller       = net.fusejna.XattrFiller
  type XattrListFiller   = net.fusejna.XattrListFiller

  def tryFuse(body: => Unit): Int = Try(body).fold(_.toErrno, constV(eok))

  def alreadyExists()  = -EEXIST
  def doesNotExist()   = -ENOENT
  def eok()            = 0
  def isMac            = scala.util.Properties.isMac
  def isNotValid()     = -EINVAL
  def notImplemented() = -ENOSYS
  def notSupported()   = notImplemented()

  implicit class ThrowableOps(private val t: Throwable) extends AnyVal {
    def toErrno: Int = t match {
      case _: FileAlreadyExistsException    => alreadyExists()
      case _: NoSuchFileException           => doesNotExist()
      case _: IllegalArgumentException      => isNotValid()
      case _: UnsupportedOperationException => notImplemented()
      case _: DirectoryNotEmptyException    => -ENOTEMPTY
      case _: SizeLimitExceededException    => -EFBIG
      case _: AccessDeniedException         => -EACCES
      case _: jio.IOException               => -EIO
      case _                                => -EIO
    }
  }

  def addUnmountHook(fs: FuseFs): Unit = scala.sys.addShutdownHook(if (fs.isMounted) fs.unmountTry())

  // see also: "allow_recursion", "nolocalcaches", "auto_xattr", "sparse"
  def defaultOptions: Vector[String] =
    if (isMac)
    /**/ Vector("-o", "direct_io,default_permissions,negative_vncache")
    else Vector("-o", "direct_io,default_permissions")

  type FuseCompatibleFs = Filesystem {
    type M[A] = Result[A]
    type Path = String
    type Name = String
    type IO   = Array[Byte]
  }

  implicit class NodeTypeOps(val nodeType: NodeType) extends AnyVal {
    def asFuse = nodeType match {
      case File => Node.File
      case Dir  => Node.Dir
      case Link => Node.Link
    }
  }
}
