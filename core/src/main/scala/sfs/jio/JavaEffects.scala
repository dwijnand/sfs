package sfs
package jio

import api._

trait JavaEffects {
  type M[_]

  def success[A](body: => A): M[A] = Try(body).fold(error, run)

  def error[A]: Throwable => M[A]

  protected def run[A]: A => M[A]
}
