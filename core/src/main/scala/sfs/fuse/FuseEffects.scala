package sfs
package fuse

import java.nio.file._
import javax.naming.SizeLimitExceededException
import jio._

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

