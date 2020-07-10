package scitzen.parser

import scitzen.parser.MacroCommand.{Emph, Strong}
import scitzen.parser.Sast._

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
        case Macro(Strong | Emph, attributes) => attributes.target
        case Macro(command, attributes)       => ""
        case InlineText(string)               => string
      }.mkString("").trim
    }
  }
  case class Section(title: Text, prefix: String, attributes: Attributes) extends Sast {
    def ref: String = attributes.named.getOrElse("label", title.str)
  }
  case class SMacro(call: Macro) extends Sast {
    override def attributes: Attributes = call.attributes
  }
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

object ListConverter {

  def splitted[ID, Item](items: Seq[(ID, Item)]): Seq[(Item, Seq[Item])] =
    items.toList match {
      case Nil => Nil
      case (marker, item) :: tail =>
        val (take, drop) = tail.span { case (c, _) => marker != c }
        (item -> take.map(_._2)) +: splitted(drop)
    }

  def listtoSast(items: Seq[ListItem]): Slist = {
    /* defines which characters are distinguishing list levels */
    def norm(m: String) = m.replaceAll("""[^\s\*\.•\-]""", "")

    val split = splitted(items.map(i => (norm(i.marker), i)))

    if (split.isEmpty) Slist(Nil) else otherList(split)
  }

  private def otherList(split: Seq[(ListItem, Seq[ListItem])]): Slist = {
    val listItems = split.map {
      case (item, children) =>
        val itemSast    = item.text.content.asInstanceOf[Paragraph].content
        val contentSast = item.content
        val childSasts  = if (children.isEmpty) None else Some(listtoSast(children))
        SlistItem(item.marker, itemSast, contentSast.orElse(childSasts).getOrElse(NoContent))
    }
    Slist(listItems)
  }
}
