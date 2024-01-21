package scitzen.sast

import de.rmgk.Chain
import scitzen.parser.TimeParsers

import scala.annotation.targetName

type Sast =
  FusedList | Directive | Section | SpaceComment | Fenced | Paragraph | FusedDelimited | FusedDefinitions

object Sast {
  def nestedIterator(sast: Seq[Sast]): Iterator[Sast] = sast.iterator.flatMap(nestedIterator)
  def nestedIterator(sast: Sast): Iterator[Sast] =
    sast match
      case sast: (FusedList | Directive | Section | SpaceComment | Fenced | Paragraph) => Iterator(sast)
      case FusedDelimited(del, cont) => Iterator(sast) ++ nestedIterator(cont)
      case FusedDefinitions(items)   => Iterator(sast) ++ items.iterator.flatMap(it => nestedIterator(it.content))
}

case class FusedList(items: Seq[FusedListItem])
case class FusedListItem(head: ListAtom, rest: Seq[TextAtom | Directive], children: Seq[FusedListItem]):
  def indent: String            = head.meta.indent
  def marker: String            = head.marker
  lazy val inlines: Seq[Inline] = Paragraph(TextAtom(head.text, head.meta) +: rest).inlines
case class FusedDefinitions(items: Seq[FusedDefinitionItem])
case class FusedDefinitionItem(head: DefinitionListAtom, content: List[Sast]):
  def text: Text = head.text

sealed trait Inline
case class InlineText(str: String, quoted: Int = 0) extends Inline:
  override def toString: String = s"“$str”"

case class Directive(command: DCommand, attributes: Attributes, meta: Meta) extends Inline

case class Text(inl: Seq[Inline])(val raw: String):
  def update(inl: Seq[Inline]): Text = Text(inl)(raw)
case object Text:
  @targetName("applySingle")
  def apply(inl: Seq[Inline], raw: String): Text = Text(inl)(raw)
  def of(str: String): Text =
    if str.isEmpty then empty
    else Text(List(InlineText(str)), str)
  val empty: Text                   = Text(Nil, "")
  def synth(inl: Seq[Inline]): Text = Text(inl, "")

case class Section(titleText: Text, prefix: String, attributes: Attributes, meta: Meta):
  private def label: Option[String] = attributes.plain("label")
  val title: String =
    def plain(text: Text): String = {
      text.inl.map {
        case InlineText(string, _) => string
        case m: Directive =>
          plain(m.attributes.text)
      }.mkString("")
    }
    plain(titleText)
  val autolabel: String = label.getOrElse(title)
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
        case text: TextAtom => InlineText(text.meta.indent) +: text.text.inl :+ InlineText("\n")

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

case class TextAtom(text: Text, meta: Meta)
case class ListAtom(marker: String, text: Text, meta: Meta)
case class DefinitionListAtom(marker: String, text: Text, meta: Meta)
case class Delimiter(marker: String, command: BCommand, attributes: Attributes, meta: Meta)
