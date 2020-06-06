package scitzen.generic

import scitzen.generic.Sast._
import scitzen.parser.MacroCommand.{Emph, Strong}
import scitzen.parser._

sealed trait Sast {
  def attributes: Attributes
}

object Sast {

  case object NoContent extends Sast {
    override def attributes: Attributes = Attributes.synt()
  }

  case class Slist(children: Seq[SlistItem]) extends Sast {
    override def attributes: Attributes = Attributes.synt()
  }
  case class SlistItem(marker: String, text: Text, content: Sast)
  case class Text(inline: Seq[Inline]) {
    lazy val str = {
      inline.map {
        case Macro(Strong | Emph, attributes) => attributes.target
        case Macro(command, attributes)  => ""
        case InlineText(string)          => string
      }.mkString("").trim
    }
  }
  case class Section(title: Text, level: Int, attributes: Attributes) extends Sast {
    def ref: String = attributes.named.getOrElse("label", title.str)

  }
  case class SMacro(call: Macro) extends Sast {
    override def attributes: Attributes = call.attributes
  }
  case class SBlock(attributes: Attributes, content: BlockType) extends Sast

  sealed trait BlockType
  case class Paragraph(content: Text) extends BlockType
  case class Fenced(content: String) extends BlockType
  case class Parsed(delimiter: String, content: Seq[Sast]) extends BlockType
  case class SpaceComment(content: String) extends BlockType

}


class ListConverter(val sastConverter: SastConverter) extends AnyVal {


  def splitted[ID, Item](items: Seq[(ID, Item)]): Seq[(Item, Seq[Item])] = items.toList match {
    case Nil                    => Nil
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
    val listItems = split.map { case (item, children) =>
      val itemSast    = sastConverter.inlineString(item.text.content, item.text.cprov)
      val contentSast = item.content.map(nb => sastConverter.blockContent(nb, nb.cprov, Attributes(nb.rawAttributes, nb.cprov)))
      val childSasts  = if (children.isEmpty) None else Some(listtoSast(children))
      SlistItem(item.marker, itemSast, contentSast.orElse(childSasts).getOrElse(NoContent))
    }
    Slist(listItems)
  }
}

final case class SastConverter() {

  private val ListConverter = new ListConverter(this)


  def blockSequence(blocks: List[Block]): List[Sast] = {
    blocks.map(block)
  }


  def blockContent(block: BlockContent, prov: Prov, attributes: Attributes): Sast = {
    block match {

      case SectionTitle(level, title, rawAttributes) =>

        Section(inlineString(title, prov), level, Attributes(rawAttributes, prov))

      case ListBlock(items) => ListConverter.listtoSast(items)

      case m: Macro => SMacro(m)

      case WhitespaceBlock(space) => SBlock(attributes, SpaceComment(space))

      case NormalBlock(delimiter, command, text, cprov, attr) =>
        val proto = SBlock(attributes.append(attr).prepend(List(Attribute("", command.str))), Fenced(""))
        if (delimiter == "")
          proto.copy(content = Paragraph(inlineString(text, cprov)))
        else delimiter.charAt(0) match {
          case '`'              => proto.copy(content = Fenced(text))
          case '.'              => SBlock(proto.attributes.prepend(List(Attribute("", "text"))), Fenced(text))
          case '=' | ' ' | '\t' => proto.copy(content = Parsed(delimiter, documentString(text, cprov)))
          case other            =>
            scribe.warn(s"mismatched block $delimiter: $text")
            proto.copy(content = Fenced(text))
        }

    }
  }

  def block(bwa: Block): Sast = blockContent(bwa.content, bwa.prov, bwa.attributes)


  def documentString(blockContent: String, prov: Prov): Seq[Sast] = {
    blockSequence(Parse.document(blockContent, prov) match {
                    case Left(parsingAnnotation) => throw parsingAnnotation
                    case Right(res)              => res.toList
                  })
  }


  def inlineString(paragraphString: String, prov: Prov): Text = {
    Text(Parse.paragraph(paragraphString, prov).toTry.get)
  }
}
