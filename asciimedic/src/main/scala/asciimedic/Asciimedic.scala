package asciimedic

import fastparse.all._
import asciimedic.CommonParsers._


/** Some things from asciidoctor have no special representation in the parsed AST
  * No substitutions are performed by the parser. Including no quotes.
  * No magic attributes https://asciidoctor.org/docs/user-manual/#role .
  * Nor setting referernce https://asciidoctor.org/docs/user-manual/#assigning-document-attributes-inline .
  * Subtitle separator https://asciidoctor.org/docs/user-manual/#document-subtitle
  *
  * */
object Asciimedic {

  val header = HeaderParsers.header

  val document: Parser[Document] = P(HeaderParsers.header.? ~ BlockParsers.fullBlock.rep ~ End)
                                   .map((Document.apply _).tupled)
}

object InlineParser {
  // \ to escape newlines, + \ to escape newlines but keep newlines
  val line      = CommonParsers.line
  val titleLine = untilI(eol)
}

