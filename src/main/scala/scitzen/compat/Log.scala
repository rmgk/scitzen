package scitzen.compat

import de.rmgk.logging.{Level, Logger}

object Logging {
  val scribe: Logger = Logger(tag = "", level = Level.Info)
}
