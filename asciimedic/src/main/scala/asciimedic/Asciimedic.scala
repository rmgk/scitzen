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

  val header = HeaderParser.header

  val document         : Parser[Document] = P(HeaderParser.header.? ~ Blocks.block.rep ~ End)
                                            .map((Document.apply _).tupled)
}

object InlineParser {
  // \ to escape newlines, + \ to escape newlines but keep newlines
  val line = CommonParsers.line
  val nonEmptyLine = untilI(eol)
}


object Macros {
  val target = P(untilE("[" | saws))
  val start                       = P(identifier.! ~ ":")
  val block : Parser[BlockMacro]  = P(start ~ ":" ~ !saws ~/ target ~ Attributes.list)
                                    .map {(BlockMacro.apply _).tupled}
  val inline: Parser[InlineMacro] = P(start ~ !saws ~ target ~ Attributes.list)
                                    .map {(InlineMacro.apply _).tupled}


  /** urls are constrained macros, i.e., they may only start after a word boundary (all whitespace?) */
  object urls {
    val scheme = P("http" ~ "s".? | "ftp" | "irc" | "mailto")

    val url: Parser[InlineMacro] = P(scheme.! ~ ":" ~/ Attributes.value ~ Attributes.list.?)
                                   .map {case (s, t, a) => InlineMacro("link", s"$s:$t", a.getOrElse(Nil))}
  }

}

object Attributes {
  val open                             = "["
  val close                            = "]"
  val entry        : Parser[Attribute] = P(":" ~ ("!".? ~ identifier ~ "!".?).! ~ ":" ~/ InlineParser.line.! ~ eol)
                                         .map { case (id, v) => Attribute(id, v) }
  val reference    : Parser[AttrRef]   = P("{" ~/ identifier.! ~ "}")
                                         .map(AttrRef.apply)
  val equals                           = P(aws ~ "=" ~ aws)
  // https://asciidoctor.org/docs/user-manual/#named-attribute
  // tells us that unquoted attribute values may not contain spaces, however this seems to be untrue in practice
  // however, in the hope of better error messages, we will not allow newlines
  val unquotedValue: Parser[String]    = P(untilE("," | close | eol))
  val value        : Parser[String]    = P(quoted("\"") | quoted("'") | unquotedValue)
  val listDef      : Parser[Attribute] = P(identifier ~ equals ~ value)
                                         .map { case (id, v) => Attribute(id, v) }
  val listValue    : Parser[Attribute] = P(value)
                                         .map(v => Attribute("", v))
  val inList                           = P(listDef | listValue)
  val list         : Parser[Seq[Attribute]]
                                       = P(open ~/ aws ~ inList.rep(sep = aws ~ "," ~ aws) ~ ",".? ~ aws ~ close)
  val line         : Parser[Seq[Attribute]] = P(list ~ swsLine)
}

object Sections {
  val title = P("=".rep(2).! ~ InlineParser.nonEmptyLine)
              .map { case (level, str) => SectionTitle(level.length - 1, str) }
}

object Paragraphs {
  val quoteChars               = "_*`^~"
  val constrainedQuote         = P(CharIn(quoteChars))
  val escaped                  = P("\\" ~ (Macros.start | Attributes.reference).!)
                                 .map(InlineText)
  val text                                    = P((iws ~ untilE(saws) ~ iws).!)
                                                .map(InlineText)
  val singleNewline            = P(!(eol ~ iwsLine) ~ eol).map(_ => InlineText("\n"))
  val token: Parser[Inline]    = P(escaped |
                                   Macros.urls.url |
                                   Macros.inline |
                                   Attributes.reference |
                                   text |
                                   singleNewline)

  val inlineSequence: Parser[Seq[Inline]] = P(token.rep(min = 1) ~ swsLine ~ iwsLine)

  val block: Parser[NormalBlock] = P(untilI(End | newlineCharacter ~ iwsLine)).map(NormalBlock(BlockType.Paragraph, _))
}


object Blocks {

  val title = P("." ~ !(" " | "...") ~ InlineParser.nonEmptyLine)

  val horizontalRule: Parser[BlockMacro] = P(("'''" | "---" | "- - -" | "***" | "* * *").!)
                                           .map(BlockMacro.apply("horizontal-rule", _, Nil))
  val pageBreak     : Parser[BlockMacro] = P("<<<".!).map(BlockMacro.apply("page-break", _, Nil))

  val whitespaceBlock: Parser[NormalBlock] = P((sws ~ eol | newlineCharacter).rep(min = 1).!)
                                             .map(NormalBlock(BlockType.Whitespace, _))

  val alternatives: Parser[Block] = P(whitespaceBlock |
                                      Lists.list |
                                      Delimited.full |
                                      horizontalRule |
                                      Sections.title |
                                      Macros.block |
                                      Paragraphs.block).log()

  val block: Parser[Block] = P(Attributes.line.rep ~ title.? ~ Attributes.line.rep ~ alternatives).log()
                             .map {
                               case (Nil, None, Nil, content)         => content
                               case (attrs1, stitle, attrs2, content) =>
                                 BlockWithAttributes(content, attrs1 ++ attrs2, stitle)
                             }

  object Delimited {
    val normalDelimiters         = "/=-.+_*"
    val normalStart              = P(normalDelimiters.map(c => c.toString.rep(4)).reduce(_ | _))
    val anyStart: Parser[String] = P((normalStart | "--" | "```" | ("|" ~ "=".rep(3))).! ~ iwsLine)

    val full: Parser[NormalBlock] = P(
      (anyStart ~/ Pass).flatMap { delimiter =>
        untilI(eol ~ delimiter, min = 0).map(content => NormalBlock(BlockType.Delimited(delimiter), content))
      }
    )
  }

  object Lists {
    val listItemMarker = P("*".rep(1) ~ " ")
    val listContent    = P(untilE(eol ~ (iwsLine | listItemMarker)))
    val listItem       = P((listItemMarker.! ~/ listContent.!)
                           .map((ListItem.apply _).tupled)).log()
    val list           = P(listItem.rep(1, sep = aws ~ Pass)
                           .map(ListBlock) ~ aws)
  }
}


/*

from http://asciidoc.org/userguide.html#_block_elements

The AsciiDoc block structure can be informally summarized as follows [1]:

Document      ::= (Header?,Preamble?,Section*)
Header        ::= (Title,(AuthorInfo,RevisionInfo?)?)
AuthorInfo    ::= (FirstName,(MiddleName?,LastName)?,EmailAddress?)
RevisionInfo  ::= (RevisionNumber?,RevisionDate,RevisionRemark?)
Preamble      ::= (SectionBody)
Section       ::= (Title,SectionBody?,(Section)*)
SectionBody   ::= ((BlockTitle?,Block)|BlockMacro)+
Block         ::= (Paragraph|DelimitedBlock|List|Table)
List          ::= (BulletedList|NumberedList|LabeledList|CalloutList)
BulletedList  ::= (ListItem)+
NumberedList  ::= (ListItem)+
CalloutList   ::= (ListItem)+
LabeledList   ::= (ListEntry)+
ListEntry     ::= (ListLabel,ListItem)
ListLabel     ::= (ListTerm+)
ListItem      ::= (ItemText,(List|ListParagraph|ListContinuation)*)


http://asciidoc.org/userguide.html#_document_processing

When a block element is encountered asciidoc(1) determines the type of block
by checking in the following order (first to last):
(section) Titles, BlockMacros, Lists, DelimitedBlocks, Tables,
AttributeEntrys, AttributeLists, BlockTitles, Paragraphs.




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