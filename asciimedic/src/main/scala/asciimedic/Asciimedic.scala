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

  val document         : Parser[Document] = P(HeaderParsers.header.? ~ BlockParsers.fullBlock.rep ~ End)
                                            .map((Document.apply _).tupled)
}

object InlineParser {
  // \ to escape newlines, + \ to escape newlines but keep newlines
  val line      = CommonParsers.line
  val titleLine = untilI(eol)
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