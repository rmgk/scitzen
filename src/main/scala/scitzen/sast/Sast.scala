package scitzen.sast

import scitzen.parser.TimeParsers

type Sast = FusedList | Directive | Section | SpaceComment | Paragraph | Block | FusedDefinitions

case class FusedList(items: Seq[FusedListItem])
case class FusedListItem(head: Container[ListAtom], rest: Seq[Container[Text | Directive]]):
  def indent: String            = head.indent
  def marker: String            = head.content.marker
  lazy val paragraph: Paragraph = Paragraph(Container("", Text(head.content.text), head.prov) +: rest)
case class FusedDefinitions(items: Seq[FusedDefinitionItem])
case class FusedDefinitionItem(head: Container[DefinitionListAtom], content: List[Sast]):
  def text: Text = Text(head.content.text)

sealed trait Inline
case class InlineText(str: String, quoted: Int = 0) extends Inline:
  override def toString: String = s"“$str”"

case class Directive(command: DCommand, attributes: Attributes)(val prov: Prov) extends Inline

case class Text(inl: Seq[Inline]) {
  def plainString: String = {
    inl.map {
      case InlineText(string, _) => string
      case m: Directive =>
        m.attributes.text.plainString
    }.mkString("")
  }

  def fuse: Text = {
    def rec(rem: Seq[Inline], acc: List[Inline]): List[Inline] =
      rem match
        case Seq(InlineText(a, aq), InlineText(b, bq), rest*) if aq == bq =>
          rec(InlineText(s"$a$b", aq) +: rest, acc)
        case Seq(first, second, rest*) =>
          rec(rem.tail, first :: acc)
        case other =>
          (rem.toList reverse_::: acc).reverse

    Text(rec(inl, Nil))
  }
}
case object Text:
  def of(str: String): Text =
    if str.isEmpty then empty
    else Text(List(InlineText(str)))
  val empty: Text = Text(Nil)

case class Section(titleText: Text, prefix: String, attributes: Attributes):
  private def label: Option[String] = attributes.plain("label")
  val title: String                 = titleText.plainString
  val autolabel: String             = label.getOrElse(title)
  def ref: String = attributes.plain("unique ref").getOrElse { throw new IllegalStateException(s"has no ref $title") }
  lazy val language: Option[String] = attributes.plain("language").orElse(attributes.plain("lang")).map(_.trim)
  lazy val date: Option[ScitzenDateTime] = attributes.plain("date").flatMap: s =>
    TimeParsers.parseDate(s.trim)
  lazy val filename: Option[String] = attributes.plain("filename")
  lazy val level: Int = prefix match
    case "="   => -1
    case "=="  => 0
    case "#"   => 1
    case "##"  => 2
    case "###" => 3
  lazy val tags = attributes.plain("tags").getOrElse("").split(',').map(_.trim).filter(_.nonEmpty).toList

object Section:
  given ordering: Ordering[Section] =
    def counts(str: String) = (str.count(_ != '='), str.count(_ == '='))
    Ordering.by(s => counts(s.prefix))

case class Paragraph(content: Seq[Container[Text | Directive]]):
  lazy val inlines: Seq[Inline] =
    content.flatMap: cont =>
      cont.content match
        case dir: Directive => List(InlineText(cont.indent), dir, InlineText("\n"))
        case text: Text     => InlineText(cont.indent) +: text.inl :+ InlineText("\n")

case class SpaceComment(content: String):
  override def toString: String = s"SpaceComment(${content.replace("\n", "\\n")})"

trait Block {
  def command: BCommand
  def attributes: Attributes
  def prov: Prov
  def indent: String
  def withAttributes(attr: Attributes): Block
}

case class Fenced(command: BCommand, attributes: Attributes, content: String, indent: String, prov: Prov) extends Block:
  override def withAttributes(attr: Attributes): Block = copy(attributes = attr)
case class Parsed(delimiter: Container[Delimiter], content: Seq[Sast]) extends Block:
  export delimiter.content.{command, attributes}
  export delimiter.{prov, indent}
  override def withAttributes(attr: Attributes): Block =
    Parsed(delimiter.copy(content = delimiter.content.copy(attributes = attr)), content)

case class Prov(start: Int = -1, end: Int = -1)

type Atom =
  Directive | Text | Delimiter | ListAtom | Section | SpaceComment | DefinitionListAtom | Fenced
case class Container[+A <: Atom](indent: String, content: A, prov: Prov)
object Container:
  def unapply[A <: Atom](cont: Container[A]): (String, A) = (cont.indent, cont.content)

case class ListAtom(marker: String, text: Seq[Inline])
case class DefinitionListAtom(marker: String, text: Seq[Inline])
case class Delimiter(marker: String, command: BCommand, attributes: Attributes)
