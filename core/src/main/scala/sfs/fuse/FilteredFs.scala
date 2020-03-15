package sfs
package fuse

import jio._, std._

/** Forwarding filesystem which only passes through paths which match the filter. */
class FilteredFs(val fs: FuseFilesystem, cond: String => Boolean) extends ForwarderFs {
  override def readdir(path: String, df: DirectoryFiller): Int = fs.readdir(path, new Filter(df))
  class Filter(filler: DirectoryFiller) extends DirectoryFiller {
    def add(files: jIterable[String]): Boolean = filler.add(files.asScala.filter(cond).asJava)
    def add(files: String*): Boolean           = filler.add(files.filter(cond).asJava)
  }
}
