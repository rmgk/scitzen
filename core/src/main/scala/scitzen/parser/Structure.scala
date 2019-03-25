package scitzen.parser

case class Document(header: Option[Header], blocks: Seq[Block])
case class Header(title: String, authorline: String, revline: String, attributes: Seq[Attribute]) {
  lazy val attribute = attributes.filter(_.id.nonEmpty).map(a => a.id -> a.value).toMap
}

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
  lazy val role      : Seq[String]         = {
    val namedRoles = named.get("role").fold(Seq.empty[String])(_.split(',').toSeq)
    val positionalRoles = positional.filter(_.startsWith(".")).flatMap(_.split('.').toSeq)
    (namedRoles ++ positionalRoles).map(_.trim).filter(_.nonEmpty)
  }
}
case class NormalBlock(blockType: BlockType, content: String) extends Block
case class ListBlock(items: Seq[ListItem]) extends Block

case class ListItem(marker: String, content: String, continuation: Option[Block])

case class Attribute(id: String, value: String)


case class BlockMacro(command: MacroType, target: String, attributes: Seq[Attribute] = Nil) extends Block
object BlockMacro {
  def fromTuple(data: (String, String, Seq[Attribute])) =
    BlockMacro(MacroType.Adhoc(data._1), data._2, data._3)
}

sealed trait MacroType
object MacroType {
  case class SectionTitle(level: Int) extends MacroType
  case object Image extends MacroType
  object HorizontalRule extends MacroType
  object PageBreak extends MacroType
  case class Adhoc(name: String) extends MacroType
}
