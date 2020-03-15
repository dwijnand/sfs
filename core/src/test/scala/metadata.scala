package sfs
package tests

import org.junit._
import Assert._

import scala.concurrent.duration._
import api._
import Attributes._
import sfs.std.{ Empty, FileTime }

final class MetadataTests {
  implicit def emptyMtime: Empty[Mtime] = Empty(???)

  @Test
  def monadsAndMetadata(): Unit = {
    val stamp = Mtime(FileTime.seconds(123))
    var attrs = Metadata().set(stamp)
    assertEquals(attrs[Mtime], stamp)

    attrs = attrs.mapOnly { case Attribute(n: Mtime) => Attribute(n + 1.day) }
    assertEquals(attrs[Mtime], stamp + 1.day)

    attrs = attrs.set(Size(10000L)).set(stamp + 7.days)
    assertEquals(attrs[Mtime], stamp + 7.days)
  }
}
