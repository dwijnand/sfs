package sfs
package fuse

import net.fusejna.ErrorCodes._
import std._

sealed trait Result[+A] { r =>
  def fold[B](f: A => B, g: Error => B): B = r match { case Success(a) => f(a) case x: Error => g(x) }

  def flatMap[B](f: A => Result[B]): Result[B]   = fold(f, idFun)
  def withFilter(f: A => Boolean): Result[A]     = fold(a => if (f(a)) r else InputOutputError, constV(InputOutputError))
  def ifGood[T >: A](z: => Result[T]): Result[T] = fold(const1(z), constV(r))
  def orElse[T >: A](z: => Result[T]): Result[T] = fold(constV(r), const1(z))
  def toInt()(implicit ev: A => Int): Int        = fold(ev, _.toErrorCode)

  def map[B](f: A => B): Result[B]               = flatMap(a => Success(f(a)))
  def ensure(f: A =?> Unit): Result[A]           = flatMap(a => if (f.isDefinedAt(a)) r else InputOutputError)
  def orElseUse[T >: A](z: => T): Result[T]      = orElse(Success(z))
}

object Result {
  implicit def _functor: Functor[Result] = new Functor[Result] { def map[A, B](f: A => B) = _.map(f) }
}

final case class Success[A](value: A) extends Result[A]

sealed trait Error extends Result[Nothing] {
  def toErrorCode: Int = this match {
    case DoesNotExist     => doesNotExist()
    case NotValid         => isNotValid()
    case NotImplemented   => notImplemented()
    case NotEmpty         => -ENOTEMPTY
    case TooBig           => -EFBIG
    case AccessDenied     => -EACCES
    case InputOutputError => -EIO
    case _                => notImplemented()
  }
}

case object InputOutputError extends Error
case object AccessDenied     extends Error
case object TooBig           extends Error
case object NotEmpty         extends Error
case object NotImplemented   extends Error
case object NotValid         extends Error
case object DoesNotExist     extends Error
case object AlreadyExists    extends Error
case object NotSupported     extends Error

