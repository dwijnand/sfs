package sfs
package api

trait Api {
  type =?>[-A, +B] = scala.PartialFunction[A, B]
  type Buf         = java.nio.ByteBuffer
  type CTag[A]     = scala.reflect.ClassTag[A]
  type Duration    = scala.concurrent.duration.Duration
  type FileTime    = java.nio.file.attribute.FileTime
  type Iso[A]      = A => A
  type Try[+A]     = scala.util.Try[A]
  type uV          = scala.annotation.unchecked.uncheckedVariance
  type unused      = scala.annotation.unused

  type Bottom <: Nothing // better w/ inference

  val Try   = scala.util.Try
  val Using = scala.util.Using

  final def cast[A](x: Any): A                        = x.asInstanceOf[A] // risks casting to Nothing$
  final def classOf[A](implicit z: CTag[A]): Class[A] = z.runtimeClass.asInstanceOf[Class[A]]
  final def classTag[A](implicit z: CTag[A]): CTag[A] = z
  final def const[R, A](x: => A): R => A              = { lazy val a = x; _ => a }
  final def const1[R, A](x: => A): R => A             = _ => x
  final def constD[R, A](x: () => A): R => A          = _ => x()
  final def constV[R, A](x: A): R => A                = _ => x
  final def doto[A](x: A)(f: A => Unit): A            = { f(x) ; x }
  final def empty[A](implicit z: Empty[A]): A         = z.emptyValue
  final def idFun[A]: A => A                          = idFun1.asInstanceOf[A => A]
  final def memo[A](x: => A): () => A                 = { lazy val a = x; () => a }
  final def void[A]: A => Unit                        = voidFun1.asInstanceOf[A => Unit]

  private val idFun1   = (x: Any) => x
  private val voidFun1 = (_: Any) => ()
}
