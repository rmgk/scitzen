package scitzen.parser

import scitzen.outputs.AttributesToScim
import scitzen.parser.MacroCommand.{Emph, Strong}
import scitzen.parser.Sast.Text

sealed trait Sast

object Sast {

  case class Slist(children: Seq[ListItem]) extends Sast
  case class ListItem(marker: String, text: Text, content: Option[Sast])
  case class Text(inline: Seq[Inline]) {
    lazy val str = {
      inline.map {
        case Macro(Strong | Emph, attributes) => attributes.target
        case Macro(command, attributes)       => ""
        case InlineText(string)               => string
      }.mkString("")
    }
  }
  case class Section(title: Text, prefix: String, attributes: Attributes) extends Sast with Ordered[Section] {
    def ref: String = attributes.named.getOrElse("label", title.str)
    override def compare(that: Section): Int = {
      def counts(str: String) = (str.count(_ != '='), str.count(_ == '='))
      Ordering[(Int, Int)].compare(counts(prefix), counts(that.prefix))
    }
  }
  case class Macro(command: MacroCommand, attributes: Attributes) extends Inline with Sast
  case class Block(attributes: Attributes, content: BlockType) extends Sast {
    override def toString: String = s"SBlock(${content.getClass.getSimpleName}, $attributes)"
    def command: String           = attributes.positional.headOption.getOrElse("")
  }

  sealed trait BlockType
  case class Paragraph(content: Text)                      extends BlockType
  case class Fenced(content: String)                       extends BlockType
  case class Parsed(delimiter: String, content: Seq[Sast]) extends BlockType
  case class SpaceComment(content: String)                 extends BlockType

}

case class Attributes(raw: Seq[Attribute], prov: Prov) {
  lazy val positional: Seq[String]               = raw.collect { case Attribute("", value) => value.str }
  lazy val arguments: Seq[String]                = positional.dropRight(1)
  lazy val target: String                        = positional.last
  lazy val named: Map[String, String]            = raw.collect { case Attribute(id, value) if id.nonEmpty => (id, value.str) }.toMap
  def append(other: Seq[Attribute]): Attributes  = Attributes(raw ++ other, prov)
  def prepend(other: Seq[Attribute]): Attributes = Attributes(other ++ raw, prov)
  def remove(key: String): Attributes            = Attributes(raw.filterNot(_.id == key), prov)
  def updated(key: String, value: String) = {
    remove(key).append(List(Attribute.apply(key, value)))
  }
  override def toString: String =
    s"Attributes(${AttributesToScim.convert(this, spacy = false, force = true, light = false)}, $prov)"
}

object Attributes {
  def synthetic(attr: Attribute*): Attributes        = Attributes(attr, Prov())
  def target(string: String, prov: Prov): Attributes = Attribute("", string).toAttributes(prov)
}

case class Attribute(id: String, inlines: Text) {
  lazy val value: String = inlines.str
  def toAttributes(prov: Prov): Attributes = Attributes(List(this), prov)
}
object Attribute {
  def apply(id: String, value: String): Attribute = Attribute(id, Text(List(InlineText(value))))
}

case class Prov(start: Int = -1, end: Int = -1, indent: Int = 0)

sealed trait Inline
case class InlineText(str: String) extends Inline
