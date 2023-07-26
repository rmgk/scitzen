package scitzen.compat

import de.rmgk.logging.{Level, Loggable, Logger}

import java.nio.file.Path

object Logging {
  var cli: Logger = Logger(level = Level.Info)

  given [T](using inner: Loggable[T]): Loggable[List[T]] with
    override def normal(v: List[T]): String = v.map(inner.normal).toString

  given Loggable[Path] = Loggable.toStringLoggable

}
