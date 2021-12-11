package scitzen.sast

import scitzen.sast.MacroCommand.{Emph, Strong}

sealed trait Sast

case class Slist(children: Seq[ListItem]) extends Sast
case class ListItem(marker: String, text: Text, content: Option[Sast])

sealed trait Inline
case class InlineText(str: String)                                          extends Inline
case class Macro(command: MacroCommand, attributes: Attributes, prov: Prov) extends Inline with Sast

case class Text(inl: Seq[Inline]) {
  lazy val str = {
    inl.map {
      case Macro(Strong | Emph, attributes, _) => attributes.target
      case m: Macro                            => ""
      case InlineText(string)                  => string
    }.mkString("")
  }
}
case class Section(title: Text, prefix: String, attributes: Attributes, prov: Prov) extends Sast with Ordered[Section] {
  def ref: String = attributes.named.getOrElse("label", title.str)
  def id: String = attributes.named.getOrElse("label", title.str)
  override def compare(that: Section): Int = {
    def counts(str: String) = (str.count(_ != '='), str.count(_ == '='))
    Ordering[(Int, Int)].compare(counts(prefix), counts(that.prefix))
  }
}
case class Block(attributes: Attributes, content: BlockType, prov: Prov) extends Sast {
  override def toString: String = s"Block(${content.getClass.getSimpleName}, $attributes)"
  def command: String           = attributes.positional.headOption.getOrElse("")
}

sealed trait BlockType
case class Paragraph(content: Text)                      extends BlockType
case class Fenced(content: String)                       extends BlockType
case class Parsed(delimiter: String, content: Seq[Sast]) extends BlockType
case class SpaceComment(content: String)                 extends BlockType

case class Attributes(raw: Seq[Attribute]) {

  lazy val positionalT: Seq[Text] = raw.collect { case Attribute("", value) => value }
  lazy val argumentsT: Seq[Text]  = positionalT.dropRight(1)
  lazy val targetT: Text          = positionalT.last
  lazy val namedT: Map[String, Text] = raw.collect {
    case Attribute(id, value) if id.nonEmpty => (id, value)
  }.toMap

  lazy val positional: Seq[String]    = positionalT.map(_.str)
  lazy val arguments: Seq[String]     = positional.dropRight(1)
  lazy val target: String             = positional.last
  lazy val named: Map[String, String] = namedT.view.mapValues(_.str).toMap

  def append(other: Seq[Attribute]): Attributes  = Attributes(raw ++ other)
  def prepend(other: Seq[Attribute]): Attributes = Attributes(other ++ raw)
  def remove(key: String): Attributes            = Attributes(raw.filterNot(_.id == key))
  def updated(key: String, value: String) = {
    remove(key).append(List(Attribute.apply(key, value)))
  }
  def updated(attribute: Attribute) = {
    remove(attribute.id).append(List(attribute))
  }

  //override def toString: String =
  //  s"Attributes(${AttributesToScim.convert(this, spacy = false, force = true, light = false)})"
}

object Attributes {
  def target(string: String): Attributes      = Attribute("", string).toAttributes
}

case class Attribute(id: String, text: Text) {
  lazy val value: String       = text.str
  def toAttributes: Attributes = Attributes(List(this))
}
object Attribute {
  def apply(id: String, value: String): Attribute = Attribute(id, Text(List(InlineText(value))))
}

case class Prov(start: Int = -1, end: Int = -1, indent: Int = 0)
