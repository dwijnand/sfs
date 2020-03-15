package sfs
package api

trait Empty[+A] {
  def emptyValue: A
}

object Empty {
  def const[A](value: A): Empty[A]    = new Empty[A] { def emptyValue = value }
  def apply[A](value: => A): Empty[A] = new Empty[A] { def emptyValue = value }
}
