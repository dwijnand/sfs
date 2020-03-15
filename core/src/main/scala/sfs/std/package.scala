package sfs

import java.nio.{ file => jnf }
import java.nio.{ channels => jnc }
import java.util.concurrent.TimeUnit
import jnf.{ attribute => jnfa }
import scala.util.{ ChainingSyntax, Failure, Success }
import scala.sys.process.{ Process, ProcessLogger }

package object std extends ChainingSyntax {
  type =?>[-A, +B] = scala.PartialFunction[A, B]
  type CTag[A]     = scala.reflect.ClassTag[A]
  type Duration    = scala.concurrent.duration.Duration
  type Try[+A]     = scala.util.Try[A]
  type uV          = scala.annotation.unchecked.uncheckedVariance
  type unused      = scala.annotation.unused

  type jArray[A]        = Array[A with Object]
  type jClass           = java.lang.Class[_]
  type jField           = java.lang.reflect.Field
  type jFilePermissions = jSet[PosixFilePermission]
  type jIterable[+A]    = java.lang.Iterable[A @uV]
  type jIterator[+A]    = java.util.Iterator[A @uV]
  type jLineIterable    = jIterable[_ <: CharSequence]
  type jList[A]         = java.util.List[A]
  type jMap[K, V]       = java.util.Map[K, V]
  type jMethod          = java.lang.reflect.Method
  type jSet[A]          = java.util.Set[A]
  type jStream[+A]      = java.util.stream.Stream[A @uV]
  type jUri             = java.net.URI
  type jUrl             = java.net.URL

  type CopyOption          = jnf.CopyOption
  type DirStreamFilter[A]  = jnf.DirectoryStream.Filter[A]
  type FileStore           = jnf.FileStore
  type FileSystem          = jnf.FileSystem
  type FileSystemProvider  = jnf.spi.FileSystemProvider
  type FileVisitOption     = jnf.FileVisitOption
  type FileVisitor[A]      = jnf.FileVisitor[A]
  type LinkOption          = jnf.LinkOption
  type OpenOption          = jnf.OpenOption
  type Path                = jnf.Path
  type PathDirStream       = jnf.DirectoryStream[Path]
  type NoSuchFileException = jnf.NoSuchFileException

  type AnyFileAttr         = jnfa.FileAttribute[_]
  type BasicFileAttributes = jnfa.BasicFileAttributes
  type FileAttributeView   = jnfa.FileAttributeView
  type FileAttribute[A]    = jnfa.FileAttribute[A]
  type GroupPrincipal      = jnfa.GroupPrincipal
  type PosixFileAttributes = jnfa.PosixFileAttributes
  type PosixFilePermission = jnfa.PosixFilePermission
  type UserPrincipal       = jnfa.UserPrincipal

  type BufferedInputStream  = java.io.BufferedInputStream
  type BufferedReader       = java.io.BufferedReader
  type BufferedWriter       = java.io.BufferedWriter
  type ByteArrayInputStream = java.io.ByteArrayInputStream
  type Buf                  = java.nio.ByteBuffer
  type Charset              = java.nio.charset.Charset
  type File                 = java.io.File
  type FileChannel          = jnc.FileChannel
  type FileInputStream      = java.io.FileInputStream
  type FileOutputStream     = java.io.FileOutputStream
  type FileTime             = java.nio.file.attribute.FileTime
  type IOException          = java.io.IOException
  type InputStream          = java.io.InputStream
  type OutputStream         = java.io.OutputStream
  type SeekableByteChannel  = jnc.SeekableByteChannel

  val Try   = scala.util.Try
  val Using = scala.util.Using

  implicit class AnyOps[A](val x: A) {
    def id_## : Int                    = System.identityHashCode(x)
    def id_==(y: Any): Boolean         = x.asInstanceOf[AnyRef].eq(y.asInstanceOf[AnyRef])  // Calling eq on Anys.
    def side(@unused effects: Any*): A = x
  }

  implicit class TryOps[A](x: Try[A]) {
    def |(alt: => A): A                          = x.getOrElse(alt)
    def ||(alt: => Try[A]): Try[A]               = x.orElse(alt)
    def fold[B](l: Throwable => B, r: A => B): B = x match { case Success(x) => r(x) case Failure(t) => l(t) }
  }

  implicit class FunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = F.map(f)(fa)
  }

  final def cast[A](x: Any): A                          = x.asInstanceOf[A] // risks casting to Nothing$
  final def classOf[A](implicit z: CTag[A]): Class[A]   = z.runtimeClass.asInstanceOf[Class[A]]
  final def classTag[A](implicit z: CTag[A]): CTag[A]   = z
  final def const[R, A](x: => A): R => A                = { lazy val a = x; _ => a }
  final def const1[R, A](x: => A): R => A               = _ => x
  final def constD[R, A](x: () => A): R => A            = _ => x()
  final def constV[R, A](x: A): R => A                  = _ => x
  final def doto[A <: Singleton](x: A)(f: A => Unit): A = { f(x) ; x }
  final def empty[A](implicit z: Empty[A]): A           = z.emptyValue
  final def idFun[A]: A => A                            = idFun1.asInstanceOf[A => A]
  final def memo[A](x: => A): () => A                   = { lazy val a = x; () => a }
  final def void[A]: A => Unit                          = voidFun1.asInstanceOf[A => Unit]

  def exec(argv: String*): ExecResult = {
    val cmd      = argv.toVector
    var out, err = Vector[String]()
    val logger   = ProcessLogger(out :+= _, err :+= _)
    val exit     = Process(cmd, None) ! logger

    ExecResult(cmd, exit, out, err)
  }

  implicit class FileTimeOps(val x: FileTime) extends AnyVal {
    def isOlder(that: FileTime) = x.compareTo(that) < 0
    def isNewer(that: FileTime) = x.compareTo(that) > 0
    def inSeconds: Long         = x.to(TimeUnit.SECONDS)
    def inMillis: Long          = x.toMillis
    def inNanoSeconds: Long     = x.to(TimeUnit.NANOSECONDS)

    def +(amount: Duration): FileTime = FileTime.millis(inMillis + amount.toMillis)
  }

  // For example statsBy(path("/usr/bin").ls)(_.mediaType.subtype)
  //
  // 675   octet-stream
  // 96    x-shellscript
  // 89    x-perl
  // 26    x-c
  // ...
  def statsBy[A, B](xs: Seq[A])(f: A => B): Unit = {
    val counts = xs.groupMapReduce(f)(constV(1))(_ + _)
    counts.toVector.sortBy(-_._2).map { case (k, n) => "%-5s %s".format(n, k) }.foreach(println)
  }

  private val idFun1   = (x: Any) => x
  private val voidFun1 = (_: Any) => ()
}
