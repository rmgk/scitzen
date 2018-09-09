package asciimedic

import fastparse.all._

case class Header(title: String, authors: Seq[Author], attributes: Seq[Attribute])
sealed trait Block
case class BlockWithAttributes(block: Block, attributes: Seq[Seq[Attribute]], title: Option[String]) extends Block
case class BlockMacro(command: String, target: String, attributes: String) extends Block
case class Paragraph(text: String) extends Block
case class Document(header: Option[Header], blocks: Seq[Block])
case class Attribute(id: String, value: String)
case class Author(name: String, email: Option[String])
case class SectionTitle(level: Int, title: String) extends Block
case class DelimitedBlock(delimiter: String, content: String) extends Block

/** Some things from asciidoctor have no special representation in the parsed AST
  * No substitutions are performed by the parser. Including no quotes.
  * No magic attributes https://asciidoctor.org/docs/user-manual/#role .
  * Nor setting referernce https://asciidoctor.org/docs/user-manual/#assigning-document-attributes-inline .
  * Subtitle separator https://asciidoctor.org/docs/user-manual/#document-subtitle
  *
  * */
object AsciidociiParser {
  val newlineCharacter     = "\n"
  val whitespaceCharacters = " \t"
  val eol                  = P(newlineCharacter | &(End))
  val inws                 = P(CharsWhileIn(whitespaceCharacters, min = 0))
  val wseol  =P(inws ~ eol)
  val saws                 = P(CharsWhileIn(whitespaceCharacters + newlineCharacter)).opaque("<whitespace>")
  val aws                  = P(saws.?).opaque("<whitespace>")
  val letter               = P(CharPred(_.isLetter)).opaque("<letter>")

  def quoted(close: String, open: Option[String] = None): Parser[String] = {
    P(open.getOrElse(close) ~/ (("\\" ~ ("\\" | close)) | (!close ~ AnyChar)).rep.! ~/ close)
    .map { str =>
      (if (close == "\\") str else str.replace(s"\\$close", close))
      .replace("\\\\", "\\")
    }
  }

  def any[T](parser: Parser[T], more: Parser[T]*): Parser[T] = more.fold(parser)(_ | _)

  def until(closing: Parser[Unit], content: Parser[Unit] = AnyChar, min: Int = 1):Parser[String] =
    P(((!closing) ~ content).rep(min).!)


  object Identifier {
    val charInWordList  = ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z') ++ "_"
    val startIdentifier = P(CharIn(charInWordList)).opaque("<start identifier>")
    val inIdentifier    = P(CharsWhileIn(charInWordList ++ "-", 0)).opaque("<in identifier>")
    val identifier      = P((startIdentifier ~ inIdentifier).!).opaque("<identifier>")
  }

  import Identifier.identifier

  val line = P(until(eol, min = 0)).log().opaque("<line>")

  object InlineParser {
    // \ to escape newlines, + \ to escape newlines but keep newlines
    val line = AsciidociiParser.this.line
    val nonEmptyLine = until(eol) ~ eol
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
    val equals                           = P(aws ~ "=")
    // https://asciidoctor.org/docs/user-manual/#named-attribute
    // tells us that unquoted attribute values may not contain spaces, however this seems to be untrue in practice
    // however, in the hope of better error messages, we will not allow newlines
    val unquotedValue: Parser[String]    = P(until(any(",", close, eol)))
    val value        : Parser[String]    = P(aws ~ quoted("\"") | aws ~ quoted("'") | unquotedValue)
    val listDef      : Parser[Attribute] = P(identifier ~ equals ~ value)
                                           .map { case (id, v) => Attribute(id, v) }
    val listValue    : Parser[Attribute] = P(value)
                                           .map(v => Attribute("", v))
    val inList                           = P(listDef | listValue)
    val list         : Parser[Seq[Attribute]]
                                         = P(open ~/ aws ~ inList.rep(sep = aws ~ "," ~ aws) ~ ",".? ~ aws ~ close)
  }

  object Sections {
    val title = P("=".rep(2).! ~ InlineParser.nonEmptyLine)
                .map { case (level, str) => SectionTitle(level.length - 1, str) }
  }


  object Blocks {
    val paragraph: Parser[Paragraph] = P(until(eol).rep(min = 1, sep = eol).! ~ eol).log()
                                       .map(Paragraph.apply)

    val title = P("." ~ InlineParser.nonEmptyLine)

    val alternatives: Parser[Block] = P(Delimited.full | Sections.title | Macros.block | paragraph)
    val block       : Parser[Block] = P(title.? ~ Attributes.list.rep(sep = aws) ~ aws ~ alternatives).log()
                                      .map {
                                        case (None, Nil, content)     => content
                                        case (stitle, attrs, content) => BlockWithAttributes(content, attrs, stitle)
                                      }

    object Delimited {
      val normalDelimiters         = "/=-.+_*"
      val normalStart              = P(normalDelimiters.map(c => c.toString.rep(4)).reduce(_ | _))
                                     .opaque("<normal block start>")
      val anyStart: Parser[String] = P((normalStart | "--" | "```" | ("|" ~ "=".rep(3))).! ~ wseol)

      val full: Parser[DelimitedBlock] = P(
        anyStart.flatMap { delimiter =>
          until(eol ~ delimiter, min = 0).map(content => DelimitedBlock(delimiter, content))
        }
      )
    }
  }

  val document         : Parser[Document] = P(HeaderParser.header.? ~ aws ~/ Blocks.block.rep ~ End).log()
                                            .map((Document.apply _).tupled)


  object HeaderParser {
    val title     : Parser[String]      = P("=" ~/ !"=" ~/ until(eol).! ~ eol)
    val author    : Parser[Author]      = P(until(any(";", "<", eol)).! ~
                                            quoted(open = Some("<"), close = ">").?)
                                          .map { case (authorName, mail) => Author(authorName, mail) }
    // asciidoctors revision line is weird https://asciidoctor.org/docs/user-manual/#revision-number-date-and-remark
    // it is clearly not meant for automatic parsing of timestamps and overall … meh
    // authorline is a bit better, but not sure if parsing is worth it.
    val revline   : Parser[String]      = P(until(eol))
    val authorline: Parser[Seq[Author]] = P(author.rep(sep = aws ~ ";"))
    val header    : Parser[Header]      = P(title ~/ authorline ~ revline.? ~ Attributes.entry.rep(sep = aws ~/ Pass) ~ aws)
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