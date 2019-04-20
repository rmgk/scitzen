package scitzen.parser

case class Document(blocks: Seq[Block]) {
  lazy val attributes: Seq[Attribute] = blocks.collect{case Block(_, _, AttributeBlock(attr)) => attr}
  lazy val named : Map[String, String] = AttributesToMap(attributes)

}

case class Header(title: SectionTitle, attributes: Seq[Attribute]) {
  lazy val named : Map[String, String] = AttributesToMap(attributes)
}

object AttributesToMap {
  def apply(attributes: Seq[Attribute]) =
    attributes.map { case Attribute(id, value) if id.nonEmpty => (id, value) }.toMap
}

case class Author(name: String, email: Option[String])

case class Block(rawAttributes: Seq[Seq[Attribute]], prov: Prov, content: BlockContent) {
  lazy val attributes: Seq[Attribute]      = rawAttributes.flatten
  lazy val positional: Seq[String]         = attributes.collect { case Attribute("", value) => value }
  lazy val named     : Map[String, String] = AttributesToMap(attributes)
}

sealed trait BlockContent
case class WhitespaceBlock(content: String) extends BlockContent
case class NormalBlock(delimiter: String, content: String) extends BlockContent
case class ListBlock(items: Seq[ListItem]) extends BlockContent
case class AttributeBlock(attribute: Attribute) extends BlockContent
case class SectionTitle(level: Int, title: String) extends BlockContent


case class ListItem(marker: String, content: String, continuation: Option[Block])

case class Attribute(id: String, value: String)


case class BlockMacro(command: MacroType, target: String, attributes: Seq[Attribute] = Nil) extends BlockContent
object BlockMacro {
  def fromTuple(data: (String, String, Seq[Attribute])) =
    BlockMacro(MacroType.Adhoc(data._1), data._2, data._3)
}

sealed trait MacroType
object MacroType {
  object HorizontalRule extends MacroType
  object PageBreak extends MacroType
  case class Adhoc(name: String) extends MacroType
}

case class Prov(start: Int = -1, end: Int = -1)