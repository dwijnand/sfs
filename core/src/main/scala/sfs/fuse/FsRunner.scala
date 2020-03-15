package sfs
package fuse

import jio._, api._

/** Generic SFS runner. */
abstract class FsRunner {
  def runMain: Array[String] =?> Unit

  def name: String                                = getClass.shortName
  def usage: String                               = "<from> <to>"
  def start(fs: FuseFs, mountPoint: String): Unit = fs.mountForeground(path(mountPoint))

  // clicking different parts together
  private def fuseJavaFs(root: Path) = new JavaFilesystem(root, new FuseEffects).withMappedPath(path, _.to_s)

  class Rooted(val root: Path, val fs: FuseCompatibleFs) extends RootedFs {
    def this(root: Path)   = this(root, fuseJavaFs(root))
    def this(root: String) = this(path(root))
    def getName            = name
  }

  final def main(args: Array[String]): Unit =
    runMain.applyOrElse(args, (_: Array[String]) => Console.err.println(s"Usage: $name $usage"))
}
