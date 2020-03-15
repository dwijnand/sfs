package sfs
package fuse

/** Cleaning up the java-centric fuse-jna types. */
object Node {
  import net.fusejna.types.TypeMode.NodeType
  final val BlockDev = NodeType.BLOCK_DEVICE
  final val Dir      = NodeType.DIRECTORY
  final val Fifo     = NodeType.FIFO
  final val File     = NodeType.FILE
  final val Link     = NodeType.SYMBOLIC_LINK
  final val Socket   = NodeType.SOCKET
}
