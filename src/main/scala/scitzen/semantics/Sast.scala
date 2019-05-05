package scitzen.semantics

import scitzen.parser._
import scitzen.semantics.Sast._

import scala.util.control.NonFatal

sealed trait Sast

object Sast {
  case class Slist(children: Seq[SlistItem]) extends Sast
  case class SlistItem(marker: String, content: Seq[Sast])
  case class Text(inline: Seq[Inline]) extends Sast {
    lazy val str = {
      inline.map{
        case Macro(command, attributes) => ""
        case InlineQuote(q, inner) => inner
        case InlineText(string) => string
      }.mkString("").trim
    }
  }
  case class Section(title: Text, content: Seq[Sast]) extends Sast
  case class MacroBlock(call: Macro) extends Sast
  case class RawBlock(delimiter: String, content: String) extends Sast
  case class ParsedBlock(delimiter: String, content: Seq[Sast]) extends Sast
  case class AttributedBlock(attr: Block, content: Sast) extends Sast
  case class AttributeDef(attribute: Attribute) extends Sast
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

  def analyze(input: Seq[Sast]) = {
    val AnalyzeResult(a, m, t) = analyzeAll(input, None, AnalyzeResult(Nil, Nil, Nil))
    AnalyzeResult(a.reverse, m.reverse, t.reverse)
  }

  def analyzeAll(inputs: Seq[Sast], scope: Option[Target], acc: AnalyzeResult): AnalyzeResult =
    inputs.foldLeft(acc)((cacc, sast) => analyzeR(sast, scope, cacc))


  def analyzeR(input: Sast, scope: Option[Target], acc: AnalyzeResult): AnalyzeResult = input match {
    case Slist(children)   => children.foldLeft(acc){(cacc, sli) =>
      analyzeAll(sli.content, scope, acc)
    }
    case Text(inlines)     => inlines.foldLeft(acc){ (cacc, inline) => inline match {
      case m: Macro             => cacc + m
      case InlineText(str)       => cacc
      case InlineQuote(q, inner) => cacc
    } }

    case sec @ Section(title, content)         =>
      val target = Target(title.str, sec)
      analyzeAll(content, Some(target), acc + target)
    case AttributeDef(attribute)        => acc + attribute
    case MacroBlock(imacro)              => {
      val iacc = if (imacro.command == "label") acc + Target(imacro.attributes.head.value, scope.get.resolution)
                 else acc
      iacc + imacro
    }
    case ParsedBlock(delimiter, content) => analyzeAll(content, scope, acc)
    case RawBlock(_, _) => acc
    case AttributedBlock(attr, content) => analyzeR(content, scope, acc)
  }
}

class ListConverter(val sastConverter: SastConverter) extends AnyVal {
  import sastConverter.{blockContent, inlineString}

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
      val itemSast = item.content match {
        case NormalBlock("", paragraph) => inlineString(paragraph)
        case other => blockContent(other)
      }
      val childSasts = if (children.isEmpty) Nil else List(listtoSast(children))
      SlistItem(item.marker, itemSast +: childSasts)
    }
    Slist(listItems)
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
        sectionize(next, Section(title, blockSequence(inner)) :: accumulator)
    }
  }



  def blockSequence(blocks: Seq[Block]): Seq[Sast] = {
    val (abstkt, sections) = blocks.span(!_.content.isInstanceOf[SectionTitle])
    abstkt.map(block) ++ sectionize(sections, Nil)
  }


  def blockContent(b: BlockContent): Sast = {
    b match {

      case SectionTitle(level, title) =>
        throw new IllegalStateException("sections should be out already …")

      case ListBlock(items) => ListConverter.listtoSast(items)

      case AttributeBlock(attribute) => AttributeDef(attribute)

      case Macro("include", attributes) =>
        val incfile = attributes.head.value
        try {
          ParsedBlock("include", documentString(includeResolver(incfile)))
        }
        catch {
          case NonFatal(e) =>
            scribe.warn(s"failed to include $incfile")
            throw e
        }

      case m: Macro => MacroBlock(m)

      case WhitespaceBlock(space) => RawBlock("", space)

      case NormalBlock(delimiter, text) =>
        if (delimiter == "") ParsedBlock("", List(inlineString(text)))
        else delimiter.charAt(0) match {
          case '`' | '.' => RawBlock(delimiter, text)
          case '=' | ' ' | '\t' => ParsedBlock(delimiter, documentString(text))
          case other     =>
            scribe.warn(s"mismatched block $delimiter: $text")
            RawBlock(delimiter, text)
        }

    }
  }

  def block(bwa: Block): Sast = {
    val inner = blockContent(bwa.content)
    AttributedBlock(bwa, inner)
  }


  def documentString(blockContent: String): Seq[Sast] = {
    blockSequence(Parse.document(blockContent).right.get.blocks)
  }


  def inlineString(paragraphString: String): Text = {
    Text(Parse.paragraph(paragraphString).toTry.get)
  }
}
