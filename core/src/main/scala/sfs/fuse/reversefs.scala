package sfs
package fuse

import jio._

/** Reverses all the data on the filesystem. */
object reversefs extends FsRunner {
  trait Reverser extends RootedFs {
    override protected def pathBytes(path: Path): Array[Byte] = super.pathBytes(path).reverse
  }

  def runMain = { case Array(from, to) => start(new Rooted(from) with Reverser, to) }
}

