package scitzen.generic

import scitzen.generic.Sast._
import scitzen.parser._

sealed trait Sast

object Sast {
  case class Slist(children: Seq[SlistItem]) extends Sast
  case class SlistItem(marker: String, content: Seq[Sast])
  case class Text(inline: Seq[InlineProv]) {
    lazy val str = {
      inline.map(_.content).map{
        case Macro(command, attributes) => ""
        case InlineQuote(q, inner) => inner
        case InlineText(string) => string
      }.mkString("").trim
    }
  }
  object Text {
    def synt(inline: Seq[Inline]): Text = Text(inline.map(synt))
    def synt(inline: Inline): InlineProv = InlineProv(inline, Prov())
  }
  case class Section(title: Text, content: Seq[TLBlock]) extends Sast
  case class MacroBlock(call: Macro) extends Sast
  case class Paragraph(content: Text) extends Sast
  case class RawBlock(delimiter: String, content: String) extends Sast
  case class ParsedBlock(delimiter: String, content: Seq[TLBlock]) extends Sast
  case class TLBlock(attr: Attributes, prov: Prov, content: Sast)
  object TLBlock{
    def synt(content: Sast) = TLBlock(Attributes(Nil), Prov(), content)
  }
  case class AttributeDef(attribute: Attribute) extends Sast
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
    def norm(m: String) = m.replaceAll("""[^\*\.:-]""", "")

    val split = splitted(items.map(i => (norm(i.marker), i)))

    if (split.isEmpty) Slist(Nil) else otherList(split)
  }

  private def otherList(split: Seq[(ListItem, Seq[ListItem])]): Slist = {
    val listItems = split.map { case (item, children) =>
      val itemSast = blockContent(item.content)
      val childSasts = if (children.isEmpty) Nil else List(listtoSast(children))
      SlistItem(item.marker, itemSast +: childSasts)
    }
    Slist(listItems)
  }
}

final case class SastConverter() {

  private val ListConverter = new ListConverter(this)

  @scala.annotation.tailrec
  def sectionize(blocks: Seq[Block], accumulator: List[TLBlock]): Seq[TLBlock] = {
    blocks.toList match {
      case Nil => accumulator.reverse
      case section :: rest =>
        val currentSection = section.content.asInstanceOf[SectionTitle]
        val title = inlineString(currentSection.title)
        val (inner, next) = rest.span{ block =>
          block.content match {
            case innerTitle: SectionTitle => innerTitle.level > currentSection.level
            case _                    => true
          }
        }
        sectionize(next, TLBlock(section.attributes, section.prov,
                                 Section(title, blockSequence(inner))) :: accumulator)
    }
  }



  def blockSequence(blocks: Seq[Block]): Seq[TLBlock] = {
    val (abstkt, sections) = blocks.span(!_.content.isInstanceOf[SectionTitle])
    abstkt.map(block) ++ sectionize(sections, Nil)
  }


  def blockContent(blockContent: BlockContent): Sast = {
    blockContent match {

      case SectionTitle(level, title) =>
        throw new IllegalStateException("sections should be out already …")

      case ListBlock(items) => ListConverter.listtoSast(items)

      case AttributeBlock(attribute) => AttributeDef(attribute)

      case m: Macro => MacroBlock(m)

      case WhitespaceBlock(space) => RawBlock("comment", space)

      case NormalBlock(delimiter, text) =>
        if (delimiter == "") Paragraph(inlineString(text))
        else delimiter.charAt(0) match {
          case '`' | '.' => RawBlock(delimiter, text)
          case '=' | ' ' | '\t' => ParsedBlock(delimiter, documentString(text))
          case other     =>
            scribe.warn(s"mismatched block $delimiter: $text")
            RawBlock(delimiter, text)
        }

    }
  }

  def block(bwa: Block): TLBlock = {
    val inner = blockContent(bwa.content)
    TLBlock(bwa.attributes, bwa.prov, inner)
  }


  def documentString(blockContent: String): Seq[TLBlock] = {
    blockSequence(Parse.document(blockContent).right.get)
  }


  def inlineString(paragraphString: String): Text = {
    Text(Parse.paragraph(paragraphString).toTry.get)
  }
}
