package sfs
package api

import java.nio.file.{ attribute => jnfa }
import java.util.concurrent.TimeUnit

object FileTime {
  def millis(ms: Long): FileTime    = jnfa.FileTime.fromMillis(ms)
  def nanos(nanos: Long): FileTime  = jnfa.FileTime.from(nanos, TimeUnit.NANOSECONDS)
  def seconds(secs: Long): FileTime = jnfa.FileTime.from(secs, TimeUnit.SECONDS)
}
