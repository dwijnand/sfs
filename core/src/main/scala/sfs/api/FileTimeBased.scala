package sfs
package api

abstract class FileTimeBased[This](create: FileTime => This) {
  def timestamp: FileTime
  def +(amount: Duration): This = create(timestamp + amount)
}
