package scitzen.parser


case class Attributes(raw: Seq[Attribute], prov: Prov) {
  def all = raw
  lazy val positional: Seq[String]         = all.collect { case Attribute("", value) => value }
  lazy val target    : String              = positional.last
  lazy val named     : Map[String, String] = all.collect { case Attribute(id, value) if id.nonEmpty => (id, value)}.toMap
  def append(other: Seq[Attribute]): Attributes = Attributes(raw ++ other, prov)
  def remove(key: String): Attributes = Attributes(raw.filterNot(_.id == key), prov)
}

object Attributes {
  def a(attrs: Attribute, prov: Prov): Attributes = Attributes(List(attrs), prov)
  def synt(attr: Attribute*) = Attributes(attr, Prov())
}

case class Block(rawAttributes: Seq[Attribute], prov: Prov, content: BlockContent) {
  lazy val attributes: Attributes = Attributes(rawAttributes, prov)

}

sealed trait BlockContent
case class WhitespaceBlock(content: String) extends BlockContent
case class NormalBlock(delimiter: String, content: String, cprov: Prov, rawAttributes: Seq[Attribute]) extends BlockContent
object NormalBlock {
  def apply(delimiter: String, cp: (String, Prov)): NormalBlock = NormalBlock(delimiter, cp._1, cp._2, Nil)
}
case class ListBlock(items: Seq[ListItem]) extends BlockContent
case class SectionTitle(level: Int, title: String, rawAttributes: Seq[Attribute]) extends BlockContent


case class ListItem(marker: String, content: NormalBlock)

case class Attribute(id: String, value: String)

case class Prov(start: Int = -1, end: Int = -1, indent: Int = 0)

sealed trait MacroCommand
object MacroCommand {
  val (parseMap, printMap) = {
    val seq = List(
      "cite" -> Cite,
      "label" -> Label,
      "ref" -> Ref,
      "image" -> Image,
      "include" -> Include,
      "link" -> Link,
      "comment" -> Comment,
      "def" -> Def,
      )
    (seq.toMap, seq.map(p => p._2 -> p._1).toMap)
  }
  def parse(str: String): MacroCommand = parseMap.getOrElse(str, Other(str))
  def print(m: MacroCommand): String = m match {
    case Other(str) => str
    case Quote(q) => q
    case o => printMap(o)
  }

  case class Quote(q: String) extends MacroCommand
  object Cite extends MacroCommand
  object Label extends MacroCommand
  object Ref extends MacroCommand
  object Image extends MacroCommand
  object Include extends MacroCommand
  object Link extends MacroCommand
  object Comment extends MacroCommand
  object Def extends MacroCommand
  case class Other(str: String) extends MacroCommand

  implicit val codecQ: upickle.default.ReadWriter[Quote] = upickle.default.macroRW
  implicit val codecO: upickle.default.ReadWriter[Other] = upickle.default.macroRW
  implicit val codec: upickle.default.ReadWriter[MacroCommand] = upickle.default.macroRW
}

sealed trait Inline
case class Macro(command: MacroCommand, attributes: Attributes) extends Inline with BlockContent
case class InlineText(str: String) extends Inline

