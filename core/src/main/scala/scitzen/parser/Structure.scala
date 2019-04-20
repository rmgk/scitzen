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

case class Prov(start: Int = -1, end: Int = -1)




sealed trait Inline
case class Macro(command: String,
                       target: String,
                       attributes: Seq[Attribute]) extends Inline with BlockContent {
}
case class InlineText(str: String) extends Inline
case class InlineQuote(q: String, inner: String) extends Inline