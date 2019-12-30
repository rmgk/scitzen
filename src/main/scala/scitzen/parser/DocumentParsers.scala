package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._


/** Some things from asciidoctor have no special representation in the parsed AST
 * No substitutions are performed by the parser. Including no quotes.
 * No magic attributes https://asciidoctor.org/docs/user-manual/#role .
 * Nor setting referernce https://asciidoctor.org/docs/user-manual/#assigning-document-attributes-inline .
 * Subtitle separator https://asciidoctor.org/docs/user-manual/#document-subtitle
 *
 * */
object DocumentParsers {
  def document[_: P]: P[Seq[Block]] = P(BlockParsers.fullBlock.rep ~ End)
}

