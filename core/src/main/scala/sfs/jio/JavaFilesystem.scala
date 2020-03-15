package sfs
package jio

import api._, std._

class JavaFilesystem[E <: JavaEffects](root: Path, val effects: E) extends Filesystem {
  type M[A] = effects.M[A]
  type Path = std.Path
  type Name = String
  type Key  = std.Path
  type IO   = Array[Byte]

  import effects._

  def resolve(path: Path): Key = root.append(path)

  def metadata(key: Key): M[Metadata] = key match {
    case path if path.nofollow.exists => success(path.metadata)
    case path                         => error(notFound(path))
  }

  def lookup(key: Key): M[Data] = key match {
    case path if !path.nofollow.exists  => error(notFound(path))
    case path if  path.isFile           => success(File(path.readAllBytes))
    case path if  path.isDir            => success(Dir(path.ls.map(path => path.filename -> path).toMap))
    case path if  path.isLink           => success(Link(path.readlink))
    case path                           => error(new IOException("unknown data type at " + path))
  }

  private def notFound(path: Path) = new NoSuchFileException(path.to_s)
}
