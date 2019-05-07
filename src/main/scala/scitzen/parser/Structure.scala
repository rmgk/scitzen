package scitzen.parser

case class Document(blocks: Seq[Block]) {
  lazy val attributes: Attributes = Attributes(blocks.collect{case Block(_, _, AttributeBlock(attr)) => attr})
  lazy val title: String = blocks.iterator.map(_.content).collectFirst{case SectionTitle(1, title) => title}.get

}

case class Attributes(all: Seq[Attribute]) {
  lazy val positional: Seq[String]         = all.collect { case Attribute("", value) => value }
  lazy val target    : String              = positional.last
  lazy val named     : Map[String, String] = all.collect { case Attribute(id, value) if id.nonEmpty => (id, value)}.toMap
}

object Attributes {
  implicit def fromAttributeSeq(some: Seq[Attribute]): Attributes = Attributes(some)
}

case class Block(rawAttributes: Seq[Seq[Attribute]], prov: Prov, content: BlockContent) {
  lazy val attributes: Attributes      = Attributes(rawAttributes.flatten)

}

sealed trait BlockContent
case class WhitespaceBlock(content: String) extends BlockContent
case class NormalBlock(delimiter: String, content: String) extends BlockContent
case class ListBlock(items: Seq[ListItem]) extends BlockContent
case class AttributeBlock(attribute: Attribute) extends BlockContent
case class SectionTitle(level: Int, title: String) extends BlockContent


case class ListItem(marker: String, content: BlockContent)

case class Attribute(id: String, value: String)

case class Prov(start: Int = -1, end: Int = -1)


sealed trait Inline
case class Macro(command: String, attributes: Attributes) extends Inline with BlockContent
case class InlineText(str: String) extends Inline
case class InlineQuote(q: String, inner: String) extends Inline