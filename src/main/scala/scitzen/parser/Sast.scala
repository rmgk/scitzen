package scitzen.parser

import scitzen.parser.MacroCommand.{Emph, Strong}

sealed trait Sast {
  def attributes: Attributes
}

object Sast {

  case object NoContent extends Sast {
    override def attributes: Attributes = Attributes.synthetic()
  }

  case class Slist(children: Seq[SlistItem]) extends Sast {
    override def attributes: Attributes = Attributes.synthetic()
  }
  case class SlistItem(marker: String, text: Text, content: Sast)
  case class Text(inline: Seq[Inline]) {
    lazy val str = {
      inline.map {
        case SMacro(Strong | Emph, attributes) => attributes.target
        case SMacro(command, attributes)       => ""
        case InlineText(string)                => string
      }.mkString("").trim
    }
  }
  case class Section(title: Text, prefix: String, attributes: Attributes) extends Sast {
    def ref: String = attributes.named.getOrElse("label", title.str)
  }
  case class SMacro(command: MacroCommand, attributes: Attributes) extends Inline with BlockContent with Sast
  case class SBlock(attributes: Attributes, content: BlockType) extends Sast {
    override def toString: String = s"SBlock(${content.getClass.getSimpleName}, $attributes)"
    def command: String           = attributes.positional.headOption.getOrElse("")
  }

  sealed trait BlockType
  case class Paragraph(content: Text)                      extends BlockType
  case class Fenced(content: String)                       extends BlockType
  case class Parsed(delimiter: String, content: Seq[Sast]) extends BlockType
  case class SpaceComment(content: String)                 extends BlockType

}


import Sast.SBlock
import scitzen.outputs.AttributesToScim

case class Attributes(raw: Seq[Attribute], prov: Prov) {
  lazy val positional: Seq[String]               = raw.collect { case Attribute("", value) => value }
  lazy val arguments: Seq[String]                = positional.dropRight(1)
  lazy val target: String                        = positional.last
  lazy val named: Map[String, String]            = raw.collect { case Attribute(id, value) if id.nonEmpty => (id, value) }.toMap
  def append(other: Seq[Attribute]): Attributes  = Attributes(raw ++ other, prov)
  def prepend(other: Seq[Attribute]): Attributes = Attributes(other ++ raw, prov)
  def remove(key: String): Attributes            = Attributes(raw.filterNot(_.id == key), prov)
  def updated(key: String, value: String) = {
    remove(key).append(List(Attribute(key, value)))
  }
  override def toString: String =
    s"Attributes(${AttributesToScim.convert(this, spacy = false, force = true, light = false)}, $prov)"
}

object Attributes {
  def synthetic(attr: Attribute*): Attributes        = Attributes(attr, Prov())
  def target(string: String, prov: Prov): Attributes = Attribute("", string).toAttributes(prov)
}

sealed trait BlockContent
case class ListBlock(items: Seq[ListItem])                                     extends BlockContent

case class ListItem(marker: String, text: SBlock, content: Option[SBlock])
case object ListItem {
  def apply(mc: (String, SBlock)): ListItem = ListItem(mc._1, mc._2, None)
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
      "cite"    -> Cite,
      "comment" -> Comment,
      "def"     -> Def,
      "image"   -> Image,
      "include" -> Include,
      "label"   -> Label,
      "link"    -> Link,
      "ref"     -> Ref,
      "code"    -> Code,
      "emph"    -> Emph,
      "strong"  -> Strong,
      "math"    -> Math,
      ""        -> Lookup
      )
    val aliases = Map(
      "fence" -> Include,
      "_"     -> Emph,
      "`"     -> Code,
      "*"     -> Strong,
      "$"     -> Math,
      "n"     -> Lookup
      )

    (standard.toMap ++ aliases, standard.map(p => p._2 -> p._1).toMap)
  }
  def parse(str: String): MacroCommand = parseMap.getOrElse(str, Other(str))
  def print(m: MacroCommand): String =
    m match {
      case Other(str) => str
      case o          => printMap(o)
    }

  object Code                   extends MacroCommand
  object Emph                   extends MacroCommand
  object Strong                 extends MacroCommand
  object Math                   extends MacroCommand
  object Cite                   extends MacroCommand
  object Comment                extends MacroCommand
  object Def                    extends MacroCommand
  object Image                  extends MacroCommand
  object Include                extends MacroCommand
  object Label                  extends MacroCommand
  object Link                   extends MacroCommand
  object Ref                    extends MacroCommand
  object Lookup                 extends MacroCommand
  case class Other(str: String) extends MacroCommand
}

sealed trait Inline
case class InlineText(str: String)                              extends Inline
