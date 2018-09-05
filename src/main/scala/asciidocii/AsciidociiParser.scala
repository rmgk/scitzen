package asciidocii

import fastparse.all._

case class Header(title: String, attributes: Map[String, String])
sealed trait Block
case class BlockMacro(command: String, target: String, attributes: String) extends Block
case class Paragraph(text: String) extends Block
case class Document(header: Header, blocks: Seq[Block])

object AsciidociiParser {
  val nl                             = P("\n")
  val ws                             = P(CharsWhile(_.isWhitespace).opaque("<whitespace>"))
  val notNL                          = P(CharPred(_ != '\n')).opaque("<not nl>")
  val notNLS    : Parser[Unit]       = P(notNL | (notNL ~ notNLS))
  val line                           = P(CharsWhile(_ != '\n', min = 0)).opaque("<line>")
  val title                          = P("=" ~/ line.! ~ nl)
  val word                           = P(CharsWhile(_.isLetterOrDigit)).opaque("<word>")
  val attribute                      = P(":" ~/ word.! ~ ":" ~/ line.! ~ nl)
  val header    : Parser[Header]     = P(title ~/ attribute.rep).map { case (title, attr) => Header(title, attr.toMap) }
  val macroAttributes                = P("[" ~ CharsWhile(c => c != '\n' && c != ']').! ~ "]")
  val macroTarget                    = P(CharsWhile(c => c != '\n' && c != '['))
  val blockMacro: Parser[BlockMacro] = P(word.! ~ "::" ~/ macroTarget.! ~ macroAttributes)
                                       .map {(BlockMacro.apply _).tupled}
  val paragraph : Parser[Paragraph]  = P(line.rep(min = 1, sep = nl ~ Pass).!).map(Paragraph.apply)
  val block     : Parser[Block]      = P(blockMacro | paragraph)
  val document                       = P(header ~ ws ~ block.rep(sep = nl ~ ws.?) ~ ws ~ End)
                                       .map((Document.apply _).tupled)

}
