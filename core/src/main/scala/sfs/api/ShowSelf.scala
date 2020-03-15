package sfs
package api

trait ShowSelf extends Any with ShowDirect {
  override def toString = to_s
}
