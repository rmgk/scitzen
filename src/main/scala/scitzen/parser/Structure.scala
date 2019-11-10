package scitzen.parser



case class Attributes(raw: Seq[Seq[Attribute]]) {
  lazy val all = raw.flatten
  lazy val positional: Seq[String]         = all.collect { case Attribute("", value) => value }
  lazy val target    : String              = positional.last
  lazy val named     : Map[String, String] = all.collect { case Attribute(id, value) if id.nonEmpty => (id, value)}.toMap
}

object Attributes {
  implicit def fromAttributeSeq(some: Seq[Attribute]): Attributes = Attributes(List(some))
  def synt(attr: Attribute*) = Attributes(List(attr))
}

case class Block(rawAttributes: Seq[Seq[Attribute]], prov: Prov, content: BlockContent) {
  lazy val attributes: Attributes      = Attributes(rawAttributes)

}

sealed trait BlockContent
case class WhitespaceBlock(content: String) extends BlockContent
case class NormalBlock(delimiter: String, content: String) extends BlockContent
case class ListBlock(items: Seq[ListItem]) extends BlockContent
case class AttributeBlock(attribute: Attribute) extends BlockContent
case class SectionTitle(level: Int, title: String) extends BlockContent


case class ListItem(marker: String, content: NormalBlock)

case class Attribute(id: String, value: String)

case class Prov(start: Int = -1, end: Int = -1)


case class InlineProv(content: Inline, prov: Prov)
sealed trait Inline
case class Macro(command: String, attributes: Attributes) extends Inline with BlockContent
case class InlineText(str: String) extends Inline
case class InlineQuote(q: String, inner: String) extends Inline