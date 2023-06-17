package scitzen.sast

import scitzen.parser.TimeParsers
import scitzen.sast.Attribute.{Nested, Plain, Positional}
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
  def autolabel: String = attributes.named.getOrElse("label", title)
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

case class Block(attributes: Attributes, content: BlockType, prov: Prov) extends Sast {
  override def toString: String = s"Block(${content.getClass.getSimpleName}, $content, $attributes)"
  def command: String           = attributes.legacyPositional.headOption.getOrElse("")
}

sealed trait BlockType
case class Paragraph(content: Text)                      extends BlockType
case class Fenced(content: String)                       extends BlockType
case class Parsed(delimiter: String, content: Seq[Sast]) extends BlockType
case class SpaceComment(content: String)                 extends BlockType

case class Attributes(raw: Seq[Attribute]) {

  lazy val positional: Seq[Text] = raw.collect { case Positional(text, _) => text }
  lazy val argumentsT: Seq[Text] = positional.dropRight(1)
  lazy val targetT: Text         = positional.last
  lazy val named: Map[String, String] = raw.collect {
    case Plain(id, value) => (id, value)
  }.toMap
  lazy val nested: Map[String, Attributes] = raw.collect {
    case Nested(id, attr) => (id, attr)
  }.toMap

  lazy val legacyPositional: Seq[String] = positional.map(_.plainString)
  lazy val arguments: Seq[String]        = legacyPositional.dropRight(1)
  lazy val target: String                = legacyPositional.last
  lazy val text: Text                    = positional.head

  def append(other: Seq[Attribute]): Attributes  = Attributes(raw ++ other)
  def prepend(other: Seq[Attribute]): Attributes = Attributes(other ++ raw)
  def remove(key: String): Attributes = Attributes(raw.filterNot {
    case Plain(`key`, _)  => true
    case Nested(`key`, _) => true
    case other            => false
  })
  def updated(key: String, value: String) = {
    remove(key).append(List(Attribute.apply(key, value)))
  }

  // override def toString: String =
  //  s"Attributes(${AttributesToScim.convert(this, spacy = false, force = true, light = false)})"
}

object Attributes {
  def target(string: String): Attributes = Attribute("", string).toAttributes
}

sealed trait Attribute {
  def toAttributes: Attributes = Attributes(List(this))
  def id: String
}

object Attribute {
  def apply(id: String, value: String): Attribute =
    if (id.isBlank) Positional(Text(List(InlineText(value))), value) else Plain(id, value)

  case class Positional(text: Text, string: String) extends Attribute {
    override def id = ""
  }
  object Positional {
    def apply(string: String): Positional =
      Positional(Text.of(string), string)
  }

  case class Plain(id: String, value: String)      extends Attribute
  case class Nested(id: String, inner: Attributes) extends Attribute

}

case class Prov(start: Int = -1, end: Int = -1, indent: Int = 0)
