package scitzen.generic

import scitzen.generic.Sast._
import scitzen.parser.MacroCommand.Quote
import scitzen.parser._

sealed trait Sast

object Sast {
  case class Slist(children: Seq[SlistItem]) extends Sast
  case class SlistItem(marker: String, content: Seq[Sast])
  case class Text(inline: Seq[Inline]) {
    lazy val str = {
      inline.map {
        case Macro(_: Quote, attributes) => attributes.target
        case Macro(command, attributes)  => ""
        case InlineText(string)          => string
      }.mkString("").trim
    }
  }
  case class Section(title: Text, content: Seq[Sast], attributes: Attributes) extends Sast {
      def ref: String = attributes.named.getOrElse("label", title.str)

  }
  case class SMacro(call: Macro) extends Sast
  case class SBlock(attr: Attributes, content: BlockType) extends Sast

  sealed trait BlockType
  case class Paragraph(content: Text) extends BlockType
  case class Fenced(content: String) extends BlockType
  case class Parsed(delimiter: String, content: Seq[Sast]) extends BlockType
  case class SpaceComment(content: String) extends BlockType
}


class ListConverter(val sastConverter: SastConverter) extends AnyVal {

  import sastConverter.blockContent

  def splitted[ID, Item](items: Seq[(ID, Item)]): Seq[(Item, Seq[Item])] = items.toList match {
    case Nil                    => Nil
    case (marker, item) :: tail =>
      val (take, drop) = tail.span { case (c, _) => marker != c }
      (item -> take.map(_._2)) +: splitted(drop)
  }

  def listtoSast(items: Seq[ListItem]): Slist = {
    /* defines which characters are distinguishing list levels */
    def norm(m: String) = m.replaceAll("""[^\s*.:-]""", "")

    val split = splitted(items.map(i => (norm(i.marker), i)))

    if (split.isEmpty) Slist(Nil) else otherList(split)
  }

  private def otherList(split: Seq[(ListItem, Seq[ListItem])]): Slist = {
    val listItems = split.map { case (item, children) =>
      val itemSast   = blockContent(item.content, Prov(), Attributes.synt())
      val childSasts = if (children.isEmpty) Nil else List(listtoSast(children))
      SlistItem(item.marker, itemSast +: childSasts)
    }
    Slist(listItems)
  }
}

final case class SastConverter() {

  private val ListConverter = new ListConverter(this)

  @scala.annotation.tailrec
  def sectionize(blocks: Seq[Block], accumulator: List[Sast]): List[Sast] = {
    blocks.toList match {
      case Nil             => accumulator.reverse
      case section :: rest =>
        val currentSection = section.content.asInstanceOf[SectionTitle]
        val title          = inlineString(currentSection.title, section.prov)
        val (inner, next)  = rest.span { block =>
          block.content match {
            case innerTitle: SectionTitle => innerTitle.level > currentSection.level
            case _                        => true
          }
        }
        sectionize(next, Section(title,
                                 blockSequence(inner),
                                 section.attributes.append(currentSection.rawAttributes)
                                 ) :: accumulator)
    }
  }


  def blockSequence(blocks: List[Block]): List[Sast] = {
    val (abstkt, sections) = blocks.span(!_.content.isInstanceOf[SectionTitle])
    abstkt.map(block) ++ sectionize(sections, Nil)
  }


  def blockContent(block: BlockContent, prov: Prov, attributes: Attributes): Sast = {
    block match {

      case SectionTitle(level, title, _) =>
        throw new IllegalStateException("sections should be out already …")

      case ListBlock(items) => ListConverter.listtoSast(items)

      case m: Macro => SMacro(m)

      case WhitespaceBlock(space) => SBlock(attributes, SpaceComment(space))

      case NormalBlock(delimiter, text, cprov, attr) =>
        val proto = SBlock(attributes.append(attr), Fenced(""))
        if (delimiter == "")
          proto.copy(content = Paragraph(inlineString(text, cprov)))
        else delimiter.charAt(0) match {
          case '`'              => proto.copy(content = Fenced(text))
          case '.'              => SBlock(proto.attr.prepend(List(Attribute("", "text"))), Fenced(text))
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
