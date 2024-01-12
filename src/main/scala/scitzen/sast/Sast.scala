package scitzen.sast

import scitzen.parser.TimeParsers

import scala.annotation.targetName

type Sast =
  FusedList | Directive | Section | SpaceComment | Fenced | Paragraph | FusedDelimited | FusedDefinitions

case class FusedList(items: Seq[FusedListItem])
case class FusedListItem(head: ListAtom, rest: Seq[TextAtom | Directive]):
  def indent: String            = head.meta.indent
  def marker: String            = head.marker
  lazy val paragraph: Paragraph = Paragraph(TextAtom(Text(head.text), head.meta) +: rest)
case class FusedDefinitions(items: Seq[FusedDefinitionItem])
case class FusedDefinitionItem(head: DefinitionListAtom, content: List[Sast]):
  def text: Text = Text(head.text)

sealed trait Inline
case class InlineText(str: String, quoted: Int = 0) extends Inline:
  override def toString: String = s"“$str”"

case class Directive(command: DCommand, attributes: Attributes, meta: Meta) extends Inline

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

case class Section(titleText: Text, prefix: String, attributes: Attributes, meta: Meta):
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

case class Paragraph(content: Seq[TextAtom | Directive]):
  lazy val inlines: Seq[Inline] =
    content.flatMap: cont =>
      cont match
        case dir: Directive => List(InlineText(dir.meta.indent), dir, InlineText("\n"))
        case text: TextAtom => InlineText(text.meta.indent) +: text.inl :+ InlineText("\n")

case class SpaceComment(content: String, meta: Meta):
  override def toString: String = s"SpaceComment(${content.replace("\n", "\\n")})"

type Block = Fenced | FusedDelimited

extension (block: Block) {
  def attributes: Attributes = block match
    case fenced: Fenced       => fenced.attributes
    case fuse: FusedDelimited => fuse.delimiter.attributes
}

case class Fenced(command: BCommand, attributes: Attributes, content: String, meta: Meta):
  def withAttributes(attr: Attributes): Fenced = copy(attributes = attr)

case class FusedDelimited(delimiter: Delimiter, content: Seq[Sast]):
  // override def meta: Meta = delimiter.meta
  def withAttributes(attr: Attributes): FusedDelimited =
    FusedDelimited(delimiter.copy(attributes = attr), content)

case class Prov(start: Int = -1, end: Int = -1)

type Atom =
  Directive | TextAtom | Delimiter | ListAtom | Section | SpaceComment | DefinitionListAtom | Fenced

extension (atom: Atom)
  def meta: Meta = atom match
    case a: Directive          => a.meta
    case a: TextAtom           => a.meta
    case a: Delimiter          => a.meta
    case a: ListAtom           => a.meta
    case a: Section            => a.meta
    case a: SpaceComment       => a.meta
    case a: DefinitionListAtom => a.meta
    case a: Fenced             => a.meta

case class Meta(indent: String)(val prov: Prov)
object Meta:
  val synth = Meta("", Prov())
  @targetName("constructor")
  def apply(indent: String, prov: Prov): Meta = new Meta(indent)(prov)

case class TextAtom(inner: Text, meta: Meta):
  export inner.inl
  def update(inlines: Seq[Inline]) = copy(inner = Text(inlines))
case class ListAtom(marker: String, text: Seq[Inline], meta: Meta)
case class DefinitionListAtom(marker: String, text: Seq[Inline], meta: Meta)
case class Delimiter(marker: String, command: BCommand, attributes: Attributes, meta: Meta)
