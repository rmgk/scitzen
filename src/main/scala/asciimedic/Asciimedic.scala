package asciimedic

import fastparse._
import fastparse.NoWhitespace._
import asciimedic.CommonParsers._


/** Some things from asciidoctor have no special representation in the parsed AST
  * No substitutions are performed by the parser. Including no quotes.
  * No magic attributes https://asciidoctor.org/docs/user-manual/#role .
  * Nor setting referernce https://asciidoctor.org/docs/user-manual/#assigning-document-attributes-inline .
  * Subtitle separator https://asciidoctor.org/docs/user-manual/#document-subtitle
  *
  * */
object Asciimedic {

  def header[_: P] = HeaderParsers.header

  def document[_: P]: P[Document] = P(HeaderParsers.header.? ~ BlockParsers.fullBlock.rep ~ End)
                                    .map((Document.apply _).tupled)
}

object InlineParser {
  // \ to escape newlines, + \ to escape newlines but keep newlines
  def line[_: P] = CommonParsers.line
  def titleLine[_: P] = untilI(eol)
}

