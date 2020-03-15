package sfs
package api

trait Functor[F[_]] {
  def map[A, B](f: A => B): F[A] => F[B]
}
