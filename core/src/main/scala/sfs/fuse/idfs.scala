package sfs
package fuse

/** The identity filesystem. */
object idfs extends FsRunner {
  def runMain = { case Array(from, to) => start(new Rooted(from).logging, to) }
}
