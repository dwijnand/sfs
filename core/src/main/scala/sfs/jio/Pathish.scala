package sfs
package jio

import java.nio.{ file => jnf }
import jnf.{ attribute => jnfa }
import jnf.{ Files => F }
import jnfa.PosixFilePermissions.asFileAttribute
import api._, std._, Attributes._

trait Pathish[Rep] extends Any {
  def path: Path
  def asRep(p: Path): Rep

  def attributes[A <: BasicFileAttributes : CTag](): Try[A] = Try(path.nofollow.readAttributes(classOf[A]))
  def basicAttributes: BasicFileAttributes                  = attributes[BasicFileAttributes] | ??? // FIXME
  def posixAttributes: PosixFileAttributes                  = attributes[PosixFileAttributes] | ??? // FIXME

  def atime: FileTime         = basicAttributes.lastAccessTime
  def ctime: FileTime         = basicAttributes.creationTime
  def mtime: FileTime         = basicAttributes.lastModifiedTime
  def inum: Object            = basicAttributes.fileKey
  def isDir: Boolean          = path.nofollow.isDirectory
  def isFile: Boolean         = path.nofollow.isRegularFile
  def isLink: Boolean         = path.isSymbolicLink
  def isOther: Boolean        = basicAttributes.isOther
  def owner: UserPrincipal    = posixAttributes.owner
  def group: GroupPrincipal   = posixAttributes.group
  def perms: jFilePermissions = posixAttributes.permissions
  def uid: Int                = UidMethod.invoke(owner).asInstanceOf[Int]
  def gid: Int                = 0 // OMG what a hassle.
  def blockCount: Long        = (path.size + blockSize - 1) / blockSize
  def blockSize: Long         = 512 // FIXME
  def depth: Int              = path.getNameCount
  def filename: String        = path.getFileName.to_s
  def mediaType: MediaType    = MediaType(exec("file", "--brief", "--mime", "--dereference", to_s).stdout.mkString("\n"))
  def moveTo(target: Path)    = path.nofollow.move(target)

  def metadata: Metadata = {
    val metadata = Metadata(Atime(atime), Mtime(mtime), UnixPerms.fromJava(perms), Uid(uid))
         if (isFile) metadata.set(NodeType.File).set(Size(path.size)).set(BlockCount(blockCount))
    else if (isDir)  metadata.set(NodeType.Dir)
    else if (isLink) metadata.set(NodeType.Link)
    else metadata
  }

  def /(name: String): Rep      = asRep(path.resolve(name))
  def ls: Vector[Rep]           = if (isDir) Using.resource(F.list(path))(_.map(asRep).toScala(Vector)) else Vector()
  def mkdir(bits: Long): Rep    = asRep(path.createDirectory(asFileAttribute(bitsAsPermissions(bits))))
  def mkfile(bits: Long): Rep   = asRep(path.createFile(asFileAttribute(bitsAsPermissions(bits))))
  def mklink(target: Path): Rep = asRep(path.createSymbolicLink(target))
  def readlink: Rep             = asRep(path.readSymbolicLink)

  def to_s: String         = path.toString
}
