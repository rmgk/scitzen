package object scribe:
  def info(s: String)  = println(s)
  def warn(s: String)  = println(s)
  def error(s: String) = println(s)
  def debug(s: String) = ()
  def trace(s: String) = ()
