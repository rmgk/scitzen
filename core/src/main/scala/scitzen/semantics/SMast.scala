package scitzen.semantics

import scitzen.parser._
import scitzen.semantics.Sast._

sealed trait Sast

object Sast {
  case class Slist(children: Seq[SlistItem]) extends Sast
  case class SlistItem(marker: String, content: Sast, inner: Slist)
  case class Text(inline: Seq[Inline]) extends Sast
  case class Section(title: Text, content: Sast) extends Sast
  case class MacroBlock(call: Macro) extends Sast
  case class RawBlock(delimiter: String, content: String) extends Sast
  case class ParsedBlock(delimiter: String, content: Sast) extends Sast
  case class AttributedBlock(attr: Block, content: Sast) extends Sast
  case class AttributeDef(attribute: Attribute) extends Sast
  case class Sseqf(sasts: Seq[Sast]) extends Sast
  object Sseq {
    def apply(sasts: Seq[Sast]): Sseqf = {
      Sseqf(sasts.flatMap {
        case Sseqf(inner) => inner
        case other        => Seq(other)
      })
    }
  }
}

object SastAnalyzes {
  def macros(input: Sast): Seq[Macro] = input match {
    case Sseqf(sasts)                    => sasts.flatMap(macros)
    case Slist(children)                 => children.flatMap(sli => macros(sli.content) ++ macros(sli.inner))
    case Text(inlines)                    => inlines flatMap {
      case m: Macro              => List(m)
      case InlineText(str)       => Nil
      case InlineQuote(q, inner) => Nil
    }
    case Section(level, content)         => macros(content)
    case AttributeDef(attribute)        => Nil
    case MacroBlock(imacro)              => List(imacro)
    case ParsedBlock(delimiter, content) => macros(content)
    case RawBlock(_, _) => Nil
    case AttributedBlock(attr, content) => macros(content)
  }
}

object ListConverter {
  import scitzen.semantics.SastConverter.{block, inlineString}

  def splitted[ID, Item](items: Seq[(ID, Item)]): Seq[(Item, Seq[Item])] = items.toList match {
    case Nil                    => Nil
    case (marker, item) :: tail =>
      val (take, drop) = tail.span { case (c, _) => marker != c }
      (item -> take.map(_._2)) +: splitted(drop)
  }

  def listToHtml(items: Seq[ListItem]): Slist = {
    def norm(m: String) = m.replaceAll("""[^\*\.:-]""", "")

    val split = splitted(items.map(i => (norm(i.marker), i)))

    split match {
      case Nil                 => Slist(Nil)
      case (firstItem, _) :: _ =>
        val n = norm(firstItem.marker)
        if (n.startsWith(":")) definitionList(split)
        else otherList(split)
    }
  }

  private def otherList(split: Seq[(ListItem, Seq[ListItem])]): Slist = {
    val listItems = split.flatMap { case (item, contents) =>
      List(
        SlistItem(item.marker,
                  Sseq(inlineString(item.content) +:
                       item.continuation.map(block).toList),
                  listToHtml(contents)
                  ))
    }
    Slist(listItems)
  }
  private def definitionList(split: Seq[(ListItem, Seq[ListItem])]): Slist = {
    Slist(split.map { case (item, contents) => definitionListItem(item, contents) })
  }

  private def definitionListItem(item: ListItem, contents: Seq[ListItem]): SlistItem = {
    SlistItem(item.marker,
              Sseq(inlineString(item.content) +:
                   item.continuation.toList.map(block)),
              listToHtml(contents)
              )
  }
}

case class ConvertResult(sast: Sast, attributes: Seq[Attribute], macros: Seq[Macro]) {
  def ++ (other: ConvertResult): ConvertResult = {
    ConvertResult(Sseq(List(sast, other.sast)),
                  attributes ++ other.attributes,
                  macros ++ other.macros)
  }
}

object SastConverter {

  @scala.annotation.tailrec
  def sectionize(blocks: Seq[Block], accumulator: List[Section]): Seq[Section] = {
    blocks.toList match {
      case Nil => accumulator.reverse
      case section :: rest =>
        val currentSection = section.content.asInstanceOf[SectionTitle]
        val title = inlineString(currentSection.title)
        val (inner, next) = rest.span{ block =>
          block.content match {
            case innerTitle: SectionTitle => innerTitle.level < currentSection.level
            case _                    => true
          }
        }
        sectionize(next, Section(title, blockSequence(inner)) :: accumulator)
    }
  }

  def blockSequence(blocks: Seq[Block]): Sast = {
    val (abstkt, sections) = blocks.span(!_.content.isInstanceOf[SectionTitle])
    Sseq(abstkt.map(block) ++ sectionize(sections, Nil))

  }


  def blockContent(b: BlockContent): Sast = {
    b match {

      case SectionTitle(level, title) =>
        throw new IllegalStateException("sections should be out already …")

      case ListBlock(items) => ListConverter.listToHtml(items)

      case AttributeBlock(attribute) => AttributeDef(attribute)

      case m: Macro => MacroBlock(m)

      case WhitespaceBlock(_) => Sseq(Nil)

      case NormalBlock(delimiter, text) =>
        if (delimiter == "") ParsedBlock("", inlineString(text))
        else delimiter.charAt(0) match {
          case '`' | '.' => RawBlock(delimiter, text)
          case '_' | ' ' => ParsedBlock(delimiter, documentString(text))
          case other     =>
            scribe.warn(s"mismatched block $delimiter: $text")
            Sseq(Nil)
        }

    }
  }

  def block(bwa: Block): Sast = {
    val inner = blockContent(bwa.content)
    AttributedBlock(bwa, inner)
  }


  def documentString(blockContent: String): Sast = {
    blockSequence(Parse.document(blockContent).right.get.blocks)
  }


  def inlineString(paragraphString: String): Text = {
    Text(Parse.paragraph(paragraphString).toTry.get)
  }
}
