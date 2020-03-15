package sfs
package fuse

/** Only passes through paths which match the given regex. */
object filterfs extends FsRunner {
  override def usage = "<from> <to> <regex>"
  def runMain = { case Array(from, to, regex) => start(new FilteredFs(new Rooted(from), !_.matches(regex)), to) }
}
