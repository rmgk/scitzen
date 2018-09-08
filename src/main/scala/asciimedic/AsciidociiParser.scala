package asciimedic

import fastparse.all._

case class Header(title: String, authors: Seq[Author], attributes: Seq[Attribute])
sealed trait Block
case class BlockWithAttributes(block: Block, attributes: Seq[Attribute]) extends Block
case class BlockMacro(command: String, target: String, attributes: String) extends Block
case class Paragraph(text: String) extends Block
case class Document(header: Option[Header], blocks: Seq[Block])
case class Attribute(id: String, value: String)
case class Author(name: String, email: Option[String])
case class SectionTitle(level: Int, title: String) extends Block

/** Some things from asciidoctor have no special representation in the parsed AST
  * No substitutions are performed by the parser. Including no quotes.
  * No magic attributes https://asciidoctor.org/docs/user-manual/#role .
  * Nor setting referernce https://asciidoctor.org/docs/user-manual/#assigning-document-attributes-inline .
  * Subtitle separator https://asciidoctor.org/docs/user-manual/#document-subtitle
  *
  * */
object AsciidociiParser {
  val eol    = P("\n" | &(End))
  val sws    = P(CharsWhile(_.isWhitespace)).opaque("<whitespace>")
  val ws     = P(sws.?)
  val letter = P(CharPred(_.isLetter)).opaque("<letter>")

  def quoted(close: String, open: Option[String] = None): Parser[String] = {
    P(open.getOrElse(close) ~/ (("\\" ~ ("\\" | close)) | (!close ~ AnyChar)).rep.! ~/ close)
    .map { str =>
      (if (close == "\\") str else str.replace(s"\\$close", close))
      .replace("\\\\", "\\")
    }
  }

  def unquoted(closing: Parser[Unit]*): Parser[String] = {
    P(((!closing.reduce(_ | _)) ~ AnyChar).rep(1).!)
  }

  object Identifier {
    val charInWordList  = ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z') ++ "_"
    val startIdentifier = P(CharIn(charInWordList)).opaque("<start identifier>")
    val inIdentifier    = P(CharsWhileIn(charInWordList ++ "-", 0)).opaque("<in identifier>")
    val identifier      = P((startIdentifier ~ inIdentifier).!).opaque("<identifier>")
  }

  import Identifier.identifier

  val line = P((!eol ~ AnyChar).rep()).log().opaque("<line>")

  object InlineParser {
    // \ to escape newlines, + \ to escape newlines but keep newlines
    val line = AsciidociiParser.this.line
    val nonEmptyLine = unquoted(eol) ~ eol
  }


  object Macros {
    val macroAttributes           = P("[" ~ CharsWhile(c => c != '\n' && c != ']').! ~ "]")
    val macroTarget               = P(CharsWhile(c => c != '\n' && c != '['))
    val block: Parser[BlockMacro] = P(identifier.! ~ "::" ~/ macroTarget.! ~ macroAttributes).log()
                                    .map {(BlockMacro.apply _).tupled}

    val inline = P(identifier.! ~ ":" ~/ macroTarget.! ~ macroAttributes)
  }

  object Attributes {
    val open                             = "["
    val close                            = "]"
    val entry        : Parser[Attribute] = P(":" ~ ("!".? ~ identifier ~ "!".?).! ~ ":" ~/ InlineParser.line.! ~ eol)
                                           .map { case (id, v) => Attribute(id, v) }
    val reference    : Parser[String]    = P("{" ~/ identifier.! ~ "}")
    val equals                           = P(ws ~ "=")
    // https://asciidoctor.org/docs/user-manual/#named-attribute
    // tells us that unquoted attribute values may not contain spaces, however this seems to be untrue in practice
    // however, in the hope of better error messages, we will not allow newlines
    val unquotedValue: Parser[String]    = P(unquoted(",", close, eol))
    val value        : Parser[String]    = P(ws ~ quoted("\"") | ws ~ quoted("'") | unquotedValue)
    val listDef      : Parser[Attribute] = P(identifier ~ equals ~ value)
                                           .map { case (id, v) => Attribute(id, v) }
    val listValue    : Parser[Attribute] = P(value)
                                           .map(v => Attribute("", v))
    val inList                           = P(listDef | listValue)
    val list         : Parser[Seq[Attribute]]
                                         = P(open ~/ ws ~ inList.rep(sep = ws ~ "," ~ ws) ~ ",".? ~ ws ~ close)
    val optionalList : Parser[Option[Seq[Attribute]]]
                                         = P(list.map(Some(_)) | (!open).map(_ => None))
  }

  object Sections {
    val title = P("=".rep(2).! ~ InlineParser.nonEmptyLine).map{ case (level, str) => SectionTitle(level.toInt, str) }
  }


  val paragraph: Parser[Paragraph] = P((AnyChar ~ line).rep(min = 1, sep = eol).! ~ eol).log()
                                     .map(Paragraph.apply)
  val block    : Parser[Block]     = P(ws ~ Attributes.optionalList ~ ws ~ (Macros.block | paragraph)).log()
                                     .map {
                                       case (Some(attrs), block) => BlockWithAttributes(block, attrs)
                                       case (None, block)        => block
                                     }
  val document : Parser[Document]  = P(HeaderParser.header.? ~/ block.rep ~ End).log()
                                     .map((Document.apply _).tupled)


  object HeaderParser {
    val title     : Parser[String]      = P("=" ~/ !"=" ~/ line.! ~ eol)
    val author    : Parser[Author]      = P(unquoted(";", "<", eol).! ~
                                            quoted(open = Some("<"), close = ">").?)
                                          .map { case (author, mail) => Author(author, mail) }
    // asciidoctors revision line is weird https://asciidoctor.org/docs/user-manual/#revision-number-date-and-remark
    // it is clearly not meant for automatic parsing of timestamps and overall … meh
    // authorline is a bit better, but not sure if parsing is worth it.
    val revline   : Parser[String]      = P(unquoted(eol))
    val authorline: Parser[Seq[Author]] = P(author.rep(sep = ws ~ ";"))
    val header    : Parser[Header]      = P(title ~/ authorline ~ revline.? ~ Attributes.entry.rep(sep = ws ~/ Pass) ~ ws)
                                          .map { case (titlestring, al, rl, attr) => Header(titlestring, al, attr) }
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