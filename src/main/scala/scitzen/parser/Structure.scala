package scitzen.parser

import scitzen.outputs.AttributesToScim


case class Attributes(raw: Seq[Attribute], prov: Prov) {
  lazy val positional: Seq[String]         = raw.collect { case Attribute("", value) => value }
  lazy val arguments : Seq[String]         = positional.dropRight(1)
  lazy val target    : String              = positional.last
  lazy val named     : Map[String, String] = raw.collect { case Attribute(id, value) if id.nonEmpty => (id, value) }.toMap
  def append(other: Seq[Attribute]): Attributes = Attributes(raw ++ other, prov)
  def prepend(other: Seq[Attribute]): Attributes = Attributes(other ++ raw, prov)
  def remove(key: String): Attributes = Attributes(raw.filterNot(_.id == key), prov)
  def updated(key: String, value: String) = {
    remove(key).append(List(Attribute(key, value)))
  }
  override def toString: String =
    s"Attributes(${AttributesToScim.convert(this, spacy = false, force = true, light = false)}, $prov)"
}

object Attributes {
  def synthetic(attr: Attribute*): Attributes = Attributes(attr, Prov())
  def target(string: String, prov: Prov): Attributes = Attribute("", string).toAttributes(prov)

}


sealed trait BlockContent
case class WhitespaceBlock(content: String, prov: Prov) extends BlockContent
case class NormalBlock(delimiter: String, command: BlockCommand, content: String, attributes: Attributes) extends BlockContent
object NormalBlock {
  def apply(delimiter: String, cp: (String, Prov)): NormalBlock = NormalBlock(delimiter, BlockCommand(""), cp._1, Attributes(Nil, cp._2))
}
case class ListBlock(items: Seq[ListItem]) extends BlockContent
case class SectionTitle(level: Int, title: String, attributes: Attributes) extends BlockContent


case class ListItem(marker: String, text: NormalBlock, content: Option[NormalBlock])
case object ListItem {
  def apply(mc: (String, NormalBlock)): ListItem = ListItem(mc._1, mc._2, None)
}

case class Attribute(id: String, value: String) {
  def toAttributes(prov: Prov): Attributes = Attributes(List(this), prov)
}

case class Prov(start: Int = -1, end: Int = -1, indent: Int = 0)

case class BlockCommand(str: String)

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
}

sealed trait Inline
case class Macro(command: MacroCommand, attributes: Attributes) extends Inline with BlockContent
case class InlineText(str: String) extends Inline

