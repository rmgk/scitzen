package scitzen.parser

import scala.util.Try

object Adoc {

  def header(content: String): Try[Header] = Try {
    fastparse.parse(content, DocumentParsers.header(_)).get.value
  }

  def document(blockContent: String): Try[Document] = Try {
    fastparse.parse(blockContent, scitzen.parser.DocumentParsers.document(_)).get.value
  }

  def paragraph(paragraphString: String): Try[Seq[Inline]] = Try {
    fastparse.parse(paragraphString, scitzen.parser.ParagraphParsers.fullParagraph(_)).get.value
  }

}
