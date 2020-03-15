package sfs
package jio

import java.nio.file._
import javax.naming.SizeLimitExceededException
import api._, fuse._

trait JavaEffects {
  type M[_]

  def success[A](body: => A): M[A] = Try(body).fold(error, run)

  def error[A]: Throwable => M[A]

  protected def run[A]: A => M[A]
}

class FuseEffects extends JavaEffects {
  type M[A] = Result[A]

  def run[A]: A => Result[A] = Success[A]

  def error[A]: Throwable => Result[A] = { t =>
    // log(t)
    t match{
      case _: FileAlreadyExistsException    => AlreadyExists
      case _: NoSuchFileException           => DoesNotExist
      case _: IllegalArgumentException      => NotValid
      case _: UnsupportedOperationException => NotImplemented
      case _: DirectoryNotEmptyException    => NotEmpty
      case _: SizeLimitExceededException    => TooBig
      case _: AccessDeniedException         => AccessDenied
      case _: jio.IOException               => InputOutputError
      case _                                => InputOutputError
    }
  }
}

class JavaFilesystem[E <: JavaEffects](root: Path, val effects: E) extends Filesystem {
  type M[A] = effects.M[A]
  type Path = jio.Path
  type Name = String
  type Key  = jio.Path
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
