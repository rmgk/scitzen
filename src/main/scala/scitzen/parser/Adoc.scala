package scitzen.parser

import scala.util.Try

object Adoc {

  def header(content: String): Try[Header] = {
    Try {fastparse.parse(content, DocumentParsers.header(_)).get.value}
  }

}
