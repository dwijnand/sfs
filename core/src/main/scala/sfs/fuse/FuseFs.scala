package sfs
package fuse

import api._, jio._

/** Widening access so we don't have to use inheritance everywhere. */
trait FuseFs extends FuseFilesystem {
  def unmountTry(): Unit = (
    if (!isMounted)
      ()
    else if (isMac)
      exec("umount", "-f", getMountPoint.getPath).orElse(exec("diskutil", "unmount", getMountPoint.getPath))
    else
      exec("fusermount", "-u", getMountPoint.getPath)
    )

  private def doMount(mountPoint: File, blocking: Boolean): this.type = {
    addUnmountHook(this)
    super.mount(mountPoint, blocking)
    this
  }

  def fillDir(df: DirectoryFiller)(xs: Iterable[Any]): Unit = xs.foreach(x => df.add("" + x))

  def mount(mountPoint: Path): this.type           = doMount(mountPoint.toFile, blocking = false)
  def mountForeground(mountPoint: Path): this.type = doMount(mountPoint.toFile, blocking = true)

  def fuseContext: FuseContext    = super.getFuseContext
  def getOptions(): Array[String] = options.toArray

  def options: Vector[String] = fuse.defaultOptions
  def getUID(): Long          = if (isMounted) fuseContext.uid.longValue else 0
  def getGID(): Long          = if (isMounted) fuseContext.gid.longValue else 0
}
