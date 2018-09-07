package asciidocii

import fastparse.all._

case class Header(title: String, attributes: Seq[(String, String)])
sealed trait Block
case class BlockMacro(command: String, target: String, attributes: String) extends Block
case class Paragraph(text: String) extends Block
case class Document(header: Option[Header], blocks: Seq[Block])
case class AttributeName(str: String)

object AsciidociiParser {
  val eol    = P("\n" | &(End))
  val ws     = P(CharsWhile(_.isWhitespace)).opaque("<whitespace>")
  val letter = P(CharPred(_.isLetter)).opaque("<letter>")

  def quoted(close: String, open: Option[String] = None) = {
    P(open.getOrElse(close) ~ (("\\" ~ ("\\" | close)) | (!close ~ AnyChar)).rep.! ~ close)
    .map { str =>
      (if (close == "\\") str else str.replace(s"\\$close", close))
      .replace("\\\\", "\\")
    }
  }

  object Identifier {
    val charInWordList  = ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z') ++ "_"
    val startIdentifier = P(CharIn(charInWordList)).opaque("<start identifier>")
    val inIdentifier    = P(CharsWhileIn(charInWordList ++ "-", 0)).opaque("<in identifier>")
    val identifier      = P((startIdentifier ~ inIdentifier).!).opaque("<identifier>").map(AttributeName(_))
  }

  import Identifier.identifier

  val line          = P((!eol ~ AnyChar).rep()).log().opaque("<line>")
  // \ to escape newlines, + \ to escape newlines but keep newlines
  val inlineContent = P(line)

  object Attribute {

    val entry     = P(":" ~ ("!".? ~ identifier ~ "!".?).! ~ ":" ~/ inlineContent.! ~ eol)
    val reference = P("{" ~/ identifier ~ "}")
    val list      = P("[" ~/ ws ~ (identifier ~ ws ~ "=" ~ ws ~ quoted("\"")) ~ ws ~ "]")
  }

  val macroAttributes                = P("[" ~ CharsWhile(c => c != '\n' && c != ']').! ~ "]")
  val macroTarget                    = P(CharsWhile(c => c != '\n' && c != '['))
  val blockMacro: Parser[BlockMacro] = P(identifier.! ~ "::" ~/ macroTarget.! ~ macroAttributes).log()
                                       .map {(BlockMacro.apply _).tupled}
  val paragraph : Parser[Paragraph]  = P((AnyChar ~ line).rep(min = 1, sep = eol).! ~ eol).log()
                                       .map(Paragraph.apply)
  val block     : Parser[Block]      = P(blockMacro | paragraph).log()
  val document                       = P(HeaderParser.header.? ~/ block.rep ~ End).log()
                                       .map((Document.apply _).tupled)


  object HeaderParser {
    val title      = P("=" ~/ !"=" ~/ line.! ~ eol)
    val authorline = letter ~ line ~ eol

    val header: Parser[Header] = P(title ~/ authorline.? ~ Attribute.entry.rep(sep = ws ~/ Pass) ~ ws.?)
                                 .map { case (title, attr) => Header(title, attr) }
  }

}


/*

from: https://asciidoctor.org/docs/user-manual/#elements

A document can include the following block elements:

Header

Title

Author Info

First Name

Middle Name

Last Name

Email Address

Revision Info

Revision Number

Revision Date

Revision Remark

Attribute Entry

Preamble

Section

Title

Section Body

BlockId

Block Title

Block Macro

Block

Paragraph

Delimited Block

Table

List

Bulleted List

Numbered List

Labeled List

Callout List

List Entry

List Label

List Item

Item Text

List Paragraph

List Continuation

An inline element performs an operation on a subset of the content within a block element.

Inline elements include:

Quotes

Replacements

Special characters

Special words

Attribute references

Inline macros


 */