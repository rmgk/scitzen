package scitzen.parser


case class Attributes(raw: Seq[Seq[Attribute]], prov: Prov) {
  lazy val all = raw.flatten
  lazy val positional: Seq[String]         = all.collect { case Attribute("", value) => value }
  lazy val target    : String              = positional.last
  lazy val named     : Map[String, String] = all.collect { case Attribute(id, value) if id.nonEmpty => (id, value)}.toMap
}

object Attributes {
  def a(attrs: Attribute, prov: Prov): Attributes = Attributes(List(List(attrs)), prov)
  def l(attrs: Seq[Attribute], prov: Prov): Attributes = Attributes(List(attrs), prov)
  def synt(attr: Attribute*) = Attributes(List(attr), Prov())
}

case class Block(rawAttributes: Seq[Seq[Attribute]], prov: Prov, content: BlockContent) {
  lazy val attributes: Attributes = Attributes(rawAttributes, prov)

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

sealed trait MacroCommand
object MacroCommand {
  val (parseMap, printMap) = {
    val seq = List(
      "label" -> Label,
      "cite" -> Cite,
      "image" -> Image,
      "include" -> Include,
      "link" -> Link,
      "comment" -> Comment,
      )
    (seq.toMap, seq.map(p => p._2 -> p._1).toMap)
  }
  def parse(str: String): MacroCommand = parseMap.getOrElse(str, Other(str))
  def print(m: MacroCommand): String = printMap.getOrElse(m, m.toString)

  case class Quote(q: String) extends MacroCommand
  object Cite extends MacroCommand
  object Label extends MacroCommand
  object Image extends MacroCommand
  object Include extends MacroCommand
  object Link extends MacroCommand
  object Comment extends MacroCommand
  case class Other(str: String) extends MacroCommand

  implicit val codecQ: upickle.default.ReadWriter[Quote] = upickle.default.macroRW
  implicit val codecO: upickle.default.ReadWriter[Other] = upickle.default.macroRW
  implicit val codec: upickle.default.ReadWriter[MacroCommand] = upickle.default.macroRW
}

sealed trait Inline
case class Macro(command: MacroCommand, attributes: Attributes) extends Inline with BlockContent
case class InlineText(str: String) extends Inline

