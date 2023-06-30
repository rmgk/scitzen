package scitzen.sast

import scitzen.parser.TimeParsers

import java.nio.file.Path

sealed trait Sast

case class Slist(children: Seq[ListItem]) extends Sast
case class ListItem(marker: String, text: Text, content: Option[Sast])

sealed trait Inline
case class InlineText(str: String, quoted: Int = 0) extends Inline:
  require(str.nonEmpty)
case class Directive(command: DCommand, attributes: Attributes)(val prov: Prov) extends Inline with Sast

case class Text(inl: Seq[Inline]) {
  def plainString: String = {
    inl.map {
      case InlineText(string, _) => string
      case m: Directive =>
        m.attributes.text.plainString
    }.mkString("")
  }
}
case object Text:
  def of(str: String): Text =
    if str.isEmpty then empty
    else Text(List(InlineText(str)))
  val empty: Text = Text(Nil)

case class Section(titleText: Text, prefix: String, attributes: Attributes)(val prov: Prov) extends Sast:
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
  lazy val relativePath: Path =
    def genName = s"${date.map(_.full).getOrElse("")} ${title}"
    val name    = filename.getOrElse(genName.take(100))
    Path.of(scitzen.cli.Format.sluggify(s"$name.html"))
  lazy val tags = attributes.plain("tags").getOrElse("").split(',').map(_.trim).filter(_.nonEmpty).toList

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
