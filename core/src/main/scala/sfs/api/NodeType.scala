package sfs
package api

final class NodeType(`type`: String) extends ShowSelf { def to_s = `type` }
object NodeType {
  val File = new NodeType("file")
  val Dir  = new NodeType("dir")
  val Link = new NodeType("link")
}
