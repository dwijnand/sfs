package sfs

final case class ExecResult(
    argv: Vector[String],
    exitCode: Int,
    stdout: Vector[String],
    stderr: Vector[String]
) {
  def isEmpty: Boolean                       = exitCode != 0
  def orElse(alt: => ExecResult): ExecResult = if (isEmpty) alt else this

  override def toString = s"""${ argv mkString " " } => $exitCode"""
}
