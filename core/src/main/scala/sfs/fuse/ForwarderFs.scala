package sfs
package fuse

import jio._, std._

/** This makes it easy to modify or extends the behavior of an existing
 *  filesystem instance by overriding a small handful of methods.
 */
abstract class ForwarderFs extends FuseFs {
  protected def fs: FuseFilesystem

  /** The non-path methods. */
  def afterUnmount(mountPoint: File): Unit = fs.afterUnmount(mountPoint)
  def beforeMount(mountPoint: File): Unit  = fs.beforeMount(mountPoint)
  def destroy(): Unit                      = fs.destroy()
  def getName(): String                    = getClass.shortName
  def init(): Unit                         = fs.init()

  /** Conceptually these are all instance methods of a path. */
  def access(path: String, access: Int): Int                                                   = fs.access(path, access)
  def bmap(path: String, info: FileInfo): Int                                                  = fs.bmap(path, info)
  def chmod(path: String, mode: ModeInfo): Int                                                 = fs.chmod(path, mode)
  def chown(path: String, uid: Long, gid: Long): Int                                           = fs.chown(path, uid, gid)
  def create(path: String, mode: ModeInfo, info: FileInfo): Int                                = fs.create(path, mode, info)
  def fgetattr(path: String, stat: StatInfo, info: FileInfo): Int                              = fs.fgetattr(path, stat, info)
  def flush(path: String, info: FileInfo): Int                                                 = fs.flush(path, info)
  def fsync(path: String, datasync: Int, info: FileInfo): Int                                  = fs.fsync(path, datasync, info)
  def fsyncdir(path: String, datasync: Int, info: FileInfo): Int                               = fs.fsyncdir(path, datasync, info)
  def ftruncate(path: String, offset: Long, info: FileInfo): Int                               = fs.ftruncate(path, offset, info)
  def getattr(path: String, stat: StatInfo): Int                                               = fs.getattr(path, stat)
  def getxattr(path: String, xattr: String, filler: XattrFiller, size: Long, pos: Long): Int   = fs.getxattr(path, xattr, filler, size, pos)
  def link(path: String, target: String): Int                                                  = fs.link(path, target)
  def listxattr(path: String, filler: XattrListFiller): Int                                    = fs.listxattr(path, filler)
  def lock(path: String, info: FileInfo, command: FlockCommand, flock: FlockWrapper): Int      = fs.lock(path, info, command, flock)
  def mkdir(path: String, mode: ModeInfo): Int                                                 = fs.mkdir(path, mode)
  def mknod(path: String, mode: ModeInfo, dev: Long): Int                                      = fs.mknod(path, mode, dev)
  def open(path: String, info: FileInfo): Int                                                  = fs.open(path, info)
  def opendir(path: String, info: FileInfo): Int                                               = fs.opendir(path, info)
  def read(path: String, buffer: Buf, size: Long, offset: Long, info: FileInfo): Int           = fs.read(path, buffer, size, offset, info)
  def readdir(path: String, filler: DirectoryFiller): Int                                      = fs.readdir(path, filler)
  def readlink(path: String, buffer: Buf, size: Long): Int                                     = fs.readlink(path, buffer, size)
  def release(path: String, info: FileInfo): Int                                               = fs.release(path, info)
  def releasedir(path: String, info: FileInfo): Int                                            = fs.releasedir(path, info)
  def removexattr(path: String, xattr: String): Int                                            = fs.removexattr(path, xattr)
  def rename(path: String, newName: String): Int                                               = fs.rename(path, newName)
  def rmdir(path: String): Int                                                                 = fs.rmdir(path)
  def setxattr(path: String, xattr: String, value: Buf, size: Long, flags: Int, pos: Int): Int = fs.setxattr(path, xattr, value, size, flags, pos)
  def statfs(path: String, wrapper: StatvfsWrapper): Int                                       = fs.statfs(path, wrapper)
  def symlink(path: String, target: String): Int                                               = fs.symlink(path, target)
  def truncate(path: String, offset: Long): Int                                                = fs.truncate(path, offset)
  def unlink(path: String): Int                                                                = fs.unlink(path)
  def utimens(path: String, wrapper: TimeBufferWrapper): Int                                   = fs.utimens(path, wrapper)
  def write(path: String, buf: Buf, bufSize: Long, writeOffset: Long, info: FileInfo): Int     = fs.write(path, buf, bufSize, writeOffset, info)
}
