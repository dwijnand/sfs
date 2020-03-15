package sfs
package api

trait Filesystem { fs =>
  /** Allows for effects */
  type M[_]

  /** A Path is a serialization of the steps one must take from
   *  the root to a particular node in the tree. */
  type Path

  /** A Name is a single directed edge of the graph. */ // e.g. String forall(ch != '/')
  type Name

  /** A key of some kind, only meaningful within this filesystem,
   *  which uniquely pinpoints the entity to which a path resolves.
   *  It is decoupled from the name. */ // e.g. 64-bit inode long
  type Key

  /** Some means of performing I/O on a virtualized file. */ // e.g. Array[Byte]
  type IO

  /** There are a number of ways we could arrange the "primitives"
   *  here. The most important thing is to retain the decoupling at
   *  abstraction boundaries so that one does not wind up computing
   *  things too eagerly, nor caching data for too long. */
  def resolve(path: Path): Key
  def metadata(key: Key): M[Metadata]
  def lookup(key: Key): M[Data]

  sealed trait Data                               extends AnyRef
  sealed case class File(io: IO)                  extends Data
  sealed case class Dir(children: Map[Name, Key]) extends Data
  sealed case class Link(target: Path)            extends Data

  object Dir { implicit def emptyDir: Empty[Dir] = Empty(Dir(Map.empty)) }

  val isFile: Data =?> Unit = { case File(_) => }
  val isLink: Data =?> Unit = { case Link(_) => }
  val isDir : Data =?> Unit = { case Dir (_) => }

  // Experiment with transforming parts of the file system, in this case the Path
  def withMappedPath[T](decPath: T => Path, encPath: Path => T)(implicit F: Functor[M]) = new Filesystem {
    type M[A] = fs.M[A]
    type Path = T
    type Name = fs.Name
    type Key  = fs.Key
    type IO   = fs.IO

    def resolve(path: Path): Key        = fs.resolve(decPath(path))
    def metadata(key: Key): M[Metadata] = fs.metadata(key)
    def lookup(key: Key): M[Data]       = fs.lookup(key).map {
      case fs.Link(path)    => Link(encPath(path))
      case fs.File(io)      => File(io)
      case fs.Dir(children) => Dir(children)
    }
  }
}
