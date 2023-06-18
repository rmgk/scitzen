package scitzen.sast

import scitzen.parser.TimeParsers
import scitzen.sast.DCommand.{Emph, Strong}

sealed trait Sast

case class Slist(children: Seq[ListItem]) extends Sast
case class ListItem(marker: String, text: Text, content: Option[Sast])

sealed trait Inline
case class InlineText(str: String) extends Inline:
  require(str.nonEmpty)
case class Directive(command: DCommand, attributes: Attributes)(val prov: Prov) extends Inline with Sast

case class Text(inl: Seq[Inline]) {
  lazy val plainString = {
    inl.map {
      case Directive(Strong | Emph, attributes) => attributes.target
      case m: Directive                         => ""
      case InlineText(string)                   => string
    }.mkString("")
  }
}
case object Text:
  def of(str: String): Text =
    if str.isEmpty then Text(Nil)
    else Text(List(InlineText(str)))

case class Section(titleText: Text, prefix: String, attributes: Attributes)(val prov: Prov) extends Sast:
  private def label: Option[String] = attributes.named.get("label")
  val autolabel: String             = label.getOrElse(title)
  def ref: String = attributes.named.getOrElse("unique ref", { throw new IllegalStateException(s"has no ref $title") })
  lazy val language: Option[String] = attributes.named.get("language").map(_.trim)
  lazy val date: Option[ScitzenDateTime] = attributes.named.get("date").flatMap: s =>
    TimeParsers.parseDate(s.trim)
  lazy val title: String            = titleText.plainString
  lazy val filename: Option[String] = attributes.named.get("filename")

object Section:
  given ordering: Ordering[Section] =
    def counts(str: String) = (str.count(_ != '='), str.count(_ == '='))
    Ordering.by(s => counts(s.prefix))

case class Block(command: BCommand, attributes: Attributes, content: BlockType)(val prov: Prov) extends Sast

sealed trait BlockType
case class Paragraph(content: Text)                      extends BlockType
case class Fenced(content: String)                       extends BlockType
case class Parsed(delimiter: String, content: Seq[Sast]) extends BlockType
case class SpaceComment(content: String)                 extends BlockType

case class Prov(start: Int = -1, end: Int = -1, indent: Int = 0)
