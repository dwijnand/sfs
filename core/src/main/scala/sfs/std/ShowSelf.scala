package sfs
package std

trait ShowSelf extends Any with ShowDirect {
  override def toString = to_s
}
