package scitzen.parser


case class Attributes(raw: Seq[Attribute], prov: Prov) {
  def all = raw
  lazy val positional: Seq[String]         = all.collect { case Attribute("", value) => value }
  lazy val arguments : Seq[String]         = positional.dropRight(1)
  lazy val target    : String              = positional.last
  lazy val named     : Map[String, String] = all.collect { case Attribute(id, value) if id.nonEmpty => (id, value) }.toMap
  def append(other: Seq[Attribute]): Attributes = Attributes(raw ++ other, prov)
  def prepend(other: Seq[Attribute]): Attributes = Attributes(other ++ raw, prov)
  def remove(key: String): Attributes = Attributes(raw.filterNot(_.id == key), prov)
  def updated(key: String, value: String) = {
    remove(key).append(List(Attribute(key, value)))
  }
}

object Attributes {
  def a(attrs: Attribute, prov: Prov): Attributes = Attributes(List(attrs), prov)
  def synt(attr: Attribute*): Attributes = Attributes(attr, Prov())
  def target(string: String, prov: Prov): Attributes = Attributes.a(Attribute("", string), prov)

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


case class ListItem(marker: String, text: NormalBlock, content: Option[NormalBlock])
case object ListItem {
  def apply(mc: (String, NormalBlock)): ListItem = ListItem(mc._1, mc._2, None)
}

case class Attribute(id: String, value: String)

case class Prov(start: Int = -1, end: Int = -1, indent: Int = 0)

sealed trait MacroCommand
object MacroCommand {
  val (parseMap, printMap) = {
    val standard = List(
      "cite" -> Cite,
      "comment" -> Comment,
      "def" -> Def,
      "image" -> Image,
      "include" -> Include,
      "label" -> Label,
      "link" -> Link,
      "ref" -> Ref,
      "code" -> Code,
      "emph" -> Emph,
      "strong" -> Strong,
      "math" -> Math
      )
    val aliases = Map(
      "fence" -> Include,
      "_" -> Emph,
      "`" -> Code,
      "*" -> Strong,
      "$" -> Math
      )

    (standard.toMap ++ aliases, standard.map(p => p._2 -> p._1).toMap)
  }
  def parse(str: String): MacroCommand = parseMap.getOrElse(str, Other(str))
  def print(m: MacroCommand): String = m match {
    case Other(str) => str
    case o          => printMap(o)
  }

  object Code extends MacroCommand
  object Emph extends MacroCommand
  object Strong extends MacroCommand
  object Math extends MacroCommand
  object Cite extends MacroCommand
  object Comment extends MacroCommand
  object Def extends MacroCommand
  object Image extends MacroCommand
  object Include extends MacroCommand
  object Label extends MacroCommand
  object Link extends MacroCommand
  object Ref extends MacroCommand
  case class Other(str: String) extends MacroCommand

  implicit val codecO: upickle.default.ReadWriter[Other]        = upickle.default.macroRW
  implicit val codec : upickle.default.ReadWriter[MacroCommand] = upickle.default.macroRW
}

sealed trait Inline
case class Macro(command: MacroCommand, attributes: Attributes) extends Inline with BlockContent
case class InlineText(str: String) extends Inline

