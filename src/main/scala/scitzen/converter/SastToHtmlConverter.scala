package scitzen.converter

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import scalatags.generic.Bundle
import scitzen.parser.{Attribute, Inline, InlineQuote, InlineText, Macro, ScitzenDateTime}
import scitzen.semantics.Sast
import scitzen.semantics.Sast._
import scitzen.semantics.SastAnalyzes.AnalyzeResult


class SastToHtmlConverter[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT],
                                                           bibliography: Map[String, String],
                                                           analyzeResult: AnalyzeResult) {

  import bundle.all._
  import bundle.tags2.nav


  def listItemToHtml(child: SlistItem)(implicit nestingLevel: NestingLevel) = {
    li(
      sastToHtml(child.content),
      sastToHtml(child.inner)
      )
  }

  def tMeta() = {

    def timeFull(date: ScitzenDateTime): Tag = {
      //need time formatter, because to string removes seconds if all zero
      span(cls := "time", date.full)
    }

    def categoriesSpan() = {
      val categories = List("categories", "people").flatMap(analyzeResult.named.get)
                       .flatMap(_.split(","))
      span(cls := "category")(categories.map(c => stringFrag(s" $c ")): _*)
    }

    p(cls := "metadata",
      analyzeResult.date.map(timeFull).getOrElse(""),
      frag(analyzeResult.modified.map(timeFull).toList: _*),
      categoriesSpan(),
      frag(analyzeResult.named.get("folder").map(f => span(cls := "category")(stringFrag(s" in $f"))).toList: _*)
      )
  }

  def sastToHtml(b: Sast)(implicit nestingLevel: NestingLevel = new NestingLevel(1)): Frag = {
    b match {

      case inner : Sections => SeqFrag(inner.all.map(sastToHtml))

      case AttributeDef(_) => frag()

      case Text(inner) => inlineValuesToHTML(inner)

      case sec@Section(title, subsections) =>
        frag(tag("h" + nestingLevel.i)(id := title.str, inlineValuesToHTML(title.inline)),
             if (nestingLevel.i == 1) frag(tMeta(), tableOfContents(subsections)) else frag(),
             sastToHtml(subsections)(nestingLevel.inc))

      case Slist(children) =>
        if (children.isEmpty) frag()
        else {
          val listTag = if (children.head.marker.contains(".")) ol else ul
          listTag(children.map(listItemToHtml))
        }

      case MacroBlock(mcro) => mcro match {
        case Macro("image", attributes) =>
          val target = attributes.last.value
          div(cls := "imageblock",
              img(src := target))
        case Macro("label", attributes) => frag()
        case Macro("horizontal-rule", attributes) => hr
        case other =>
          scribe.warn(s"not implemented: $other")
          div(stringFrag(other.toString))
      }

      case ParsedBlock(delimiter, blockContent) =>
        if (delimiter == "") p(sastToHtml(blockContent))
        else delimiter.charAt(0) match {

          case '=' => blockquote(sastToHtml(blockContent))
          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case ' ' => sastToHtml(blockContent)
          // there is also '=' example, and '+' passthrough.
          // examples seems rather specific, and passthrough is not implemented.
          case _   => div(delimiter, br, sastToHtml(blockContent), br, delimiter)
        }

      case RawBlock(delimiter, text) =>
        if (delimiter.isEmpty) ""
        else delimiter.charAt(0) match {
          // Code listing
          // Use this for monospace, space preserving, line preserving text
          // It may wrap to fit the screen content
          case '`' => pre(code(text))
          // Literal block
          // This seems to be supposed to work similar to code? But whats the point then?
          // We interpret this as text with predetermined line wrappings
          // and no special syntax, but otherwise normally formatted.
          // This is great to represent copy&pasted posts or chat messages.
          case '.' => pre(text)
        }



      case bwa: AttributedBlock =>
        val positiontype = bwa.attr.positional.headOption
        positiontype match {
          case Some("quote") =>
            val innerHtml = sastToHtml(bwa.content)
            // for blockquote layout, see example 12 (the twitter quote)
            // http://w3c.github.io/html/textlevel-semantics.html#the-cite-element
            val bq = blockquote(innerHtml)
            // first argument is "quote" we concat the rest and treat them as a single entity
            val title = bwa.attr.positional.drop(1)
            if (title.nonEmpty) bq(cite(title))
            else bq
          case _         => sastToHtml(bwa.content)
        }
    }
  }


  private def tableOfContents(sectionContent: Sast): Frag = {
    if (analyzeResult.attributes.exists(_.id == "toc")) {
      sectionContent match {
        case Sections(abtrkt, subsections) => nav(ol(subsections.map {
          case Section(title, _) => li(a(href := s"#${title.str}", title.str))
        }))
        case other      => frag()
      }
    } else frag()
  }
  def inlineValuesToHTML(inners: Seq[Inline]): Seq[Frag] = inners.map[Frag, Seq[Frag]] {
    case InlineText(str) => str
    case InlineQuote(q, inner) =>
      //scribe.warn(s"inline quote $q: $inner; ${post.sourcePath}")
      q.head match {
      case '_' => em(inner)
      case '*' => strong(inner)
      case '`' => code(inner)
      case '$' => span(raw((scala.sys.process.Process(s"katex") #< new ByteArrayInputStream(inner.getBytes(StandardCharsets.UTF_8))).!!))
    }
    case Macro("comment", attributes) => frag()
    case Macro("ref", attributes) =>
      analyzeResult.targets.find(_.id == attributes.head.value).map {target =>
        target.resolution match {
          case Section(title, _) => a(href := s"#${title.str}", inlineValuesToHTML(title.inline))
          case other =>
            scribe.error(s"can not refer to $other")
            frag()
        }
      }
    case Macro("cite", attributes) =>
      val bibid = attributes.head.value
      frag("\u00A0", a(href := s"#$bibid", bibliography(bibid)))
    case Macro(tagname@("ins" | "del"), attributes) =>
      tag(tagname)(attributes.iterator.filter(_.id.isEmpty).map(_.value).mkString(", "))
    case Macro(protocol @ ("http" | "https" | "ftp" | "irc" | "mailto"), attributes) =>
      val target = attributes.last.value
      val linktarget = s"$protocol:$target"
      linkTo(attributes, linktarget)
    case Macro("link", attributes) =>
      val target = attributes.last.value
      linkTo(attributes, target)
    case Macro("footnote", attributes) =>
      val target = attributes.last.value
      a(title := target, "※")
    case im @ Macro(command, attributes) =>
      scribe.warn(s"inline macro “$command[$attributes]”")
      code(s"$command[${attributes.mkString(",")}]")
  }
  def linkTo(attributes: Seq[Attribute], linktarget: String) = {
    a(href := linktarget)(attributes.find(_.id == "").fold(linktarget)(_.value))
  }
}
