package scitzen.compat

import de.rmgk.logging.{Level, Logger}

object Logging {
  val cli: Logger = Logger(level = Level.Info)
}
