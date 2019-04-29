package scitzen.semantics

import scitzen.parser._
import scitzen.semantics.Sast._

import scala.util.control.NonFatal

sealed trait Sast

object Sast {
  case class Slist(children: Seq[SlistItem]) extends Sast
  case class SlistItem(marker: String, content: Sast, inner: Slist)
  case class Text(inline: Seq[Inline]) extends Sast {
    lazy val str = {
      inline.map{
        case Macro(command, attributes) => ""
        case InlineQuote(q, inner) => inner
        case InlineText(string) => string
      }.mkString("").trim
    }
  }
  case class Section(title: Text, content: Seq[Sast], children: Seq[Section]) extends Sast {
    lazy val all: Sast = Sseqf(content ++ children)
  }
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
  case class Target(id: String, resolution: Sast)
  case class AnalyzeResult(attributes: List[Attribute],
                           macros: List[Macro],
                           targets: List[Target]) {
    def +(m: Macro): AnalyzeResult = copy(macros = m :: macros)
    def +(m: Attribute): AnalyzeResult = copy(attributes = m :: attributes)
    def +(m: Target): AnalyzeResult = copy(targets = m :: targets)

    lazy val named: Map[String, String] = AttributesToMap(attributes)

    lazy val language: String = named.getOrElse("lang", "")

    lazy val date    : Option[ScitzenDateTime] = named.get("revdate")
                                                 .map(v => DateParsingHelper.parseDate(v.trim))
    lazy val modified: Option[ScitzenDateTime] = named.get("modified")
                                                 .map(m => DateParsingHelper.parseDate(m.trim))

  }

  def analyze(input: Sast) = {
    val AnalyzeResult(a, m, t) = analyzeR(input, None, AnalyzeResult(Nil, Nil, Nil))
    AnalyzeResult(a.reverse, m.reverse, t.reverse)
  }

  def analyzeR(input: Sast, scope: Option[Target], acc: AnalyzeResult): AnalyzeResult = input match {
    case Sseqf(sasts)                    => sasts.foldLeft(acc)((cacc, sast) => analyzeR(sast, scope, cacc))
    case Slist(children)                 => children.foldLeft(acc){(cacc, sli) =>
      val contentAcc = analyzeR(sli.content, scope, cacc)
      analyzeR(sli.inner, scope, contentAcc)
    }
    case Text(inlines)                    => inlines.foldLeft(acc){ (cacc, inline) => inline match {
      case m: Macro             => cacc + m
      case InlineText(str)       => cacc
      case InlineQuote(q, inner) => cacc
    } }
    case sec @ Section(title, _, _)         =>
      val target = Target(title.str, sec)
      analyzeR(sec.all, Some(target), acc + target)
    case AttributeDef(attribute)        => acc + attribute
    case MacroBlock(imacro)              => {
      val iacc = if (imacro.command == "label") acc + Target(imacro.attributes.head.value, scope.get.resolution)
                 else acc
      iacc + imacro
    }
    case ParsedBlock(delimiter, content) => analyzeR(content, scope, acc)
    case RawBlock(_, _) => acc
    case AttributedBlock(attr, content) => analyzeR(content, scope, acc)
  }
}

class ListConverter(val sastConverter: SastConverter) extends AnyVal {
  import sastConverter.{block, inlineString}

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

final class SastConverter(includeResolver: String => String) {

  private val ListConverter = new ListConverter(this)

  @scala.annotation.tailrec
  def sectionize(blocks: Seq[Block], accumulator: List[Section]): Seq[Section] = {
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
        val (content, children) = blockSequenceSections(inner)
        sectionize(next, Section(title, content, children) :: accumulator)
    }
  }



  def blockSequenceSections(blocks: Seq[Block]): (Seq[Sast], Seq[Section]) = {
    val (abstkt, sections) = blocks.span(!_.content.isInstanceOf[SectionTitle])
    (abstkt.map(block),  sectionize(sections, Nil))
  }

  def blockSequence(blocks: Seq[Block]): Sseqf = {
    val (content, children) = blockSequenceSections(blocks)
    Sseqf(content ++ children)
  }


  def blockContent(b: BlockContent): Sast = {
    b match {

      case SectionTitle(level, title) =>
        throw new IllegalStateException("sections should be out already …")

      case ListBlock(items) => ListConverter.listToHtml(items)

      case AttributeBlock(attribute) => AttributeDef(attribute)

      case Macro("include", attributes) =>
        val incfile = attributes.head.value
        try {
          documentString(includeResolver(incfile))
        }
        catch {
          case NonFatal(e) =>
            scribe.warn(s"failed to include $incfile")
            throw e
        }

      case m: Macro => MacroBlock(m)

      case WhitespaceBlock(_) => Sseq(Nil)

      case NormalBlock(delimiter, text) =>
        if (delimiter == "") ParsedBlock("", inlineString(text))
        else delimiter.charAt(0) match {
          case '`' | '.' => RawBlock(delimiter, text)
          case '=' | ' ' | '\t' => ParsedBlock(delimiter, documentString(text))
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


  def documentString(blockContent: String): Sseqf = {
    blockSequence(Parse.document(blockContent).right.get.blocks)
  }


  def inlineString(paragraphString: String): Text = {
    Text(Parse.paragraph(paragraphString).toTry.get)
  }
}
