package asciimedic


case class Document(header: Option[Header], blocks: Seq[Block])
case class Header(title: String, authors: Seq[Author], attributes: Seq[Attribute])

case class Author(name: String, email: Option[String])

sealed trait BlockType
object BlockType {
  case object Paragraph extends BlockType
  case class Delimited(delimiter: String) extends BlockType
  case object Whitespace extends BlockType
}


sealed trait Block

case class BlockWithAttributes(block        : Block,
                               rawAttributes: Seq[Seq[Attribute]],
                               title        : Option[String]
                              ) extends Block {
  lazy val attributes: Seq[Attribute]      = rawAttributes.flatten
  lazy val positional: Seq[String]         = attributes.collect { case Attribute("", value) => value }
  lazy val named     : Map[String, String] =
    attributes.collect { case Attribute(id, value) if id.nonEmpty => (id, value) }.toMap
  lazy val role: Seq[String] = {
    val namedRoles = named.get("role").fold(Seq.empty[String])(_.split(',').toSeq)
    val positionalRoles = positional.filter(_.startsWith(".")).flatMap(_.split('.').toSeq)
    (namedRoles ++ positionalRoles).map(_.trim).filter(_.nonEmpty)
  }
}
case class NormalBlock(blockType: BlockType, content: String) extends Block
case class BlockMacro(command: String, target: String, attributes: Seq[Attribute]) extends Block
case class SectionTitle(level: Int, title: String) extends Block
case class ListBlock(items: Seq[ListItem]) extends Block

case class ListItem(marker: String, content: String)

case class Attribute(id: String, value: String)


sealed trait Inline
case class InlineMacro(command: String, target: String, attributes: Seq[Attribute]) extends Inline
case class InlineText(str: String) extends Inline
case class AttrRef(id: String) extends Inline