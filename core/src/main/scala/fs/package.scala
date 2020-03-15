package sfs

import api._

package object fs {
  // This file exists to experiment with transforming parts of the file system, in this case the Path
  implicit class Wrapped[FS <: Filesystem](val orig: FS) {
    def withMappedPath[T](
        decPath: T => orig.Path,
        encPath: orig.Path => T
    )(implicit F: Functor[orig.M]) = new Filesystem {
      type M[A] = orig.M[A]
      type Path = T
      type Name = orig.Name
      type Key  = orig.Key
      type IO   = orig.IO

      def resolve(path: Path): Key        = orig.resolve(decPath(path))
      def metadata(key: Key): M[Metadata] = orig.metadata(key)
      def lookup(key: Key): M[Data]       = orig.lookup(key).map {
        case orig.Link(path)    => Link(encPath(path))
        case orig.File(io)      => File(io)
        case orig.Dir(children) => Dir(children)
      }
    }
  }
}
