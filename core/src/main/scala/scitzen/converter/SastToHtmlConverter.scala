package scitzen.converter

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import scalatags.generic.Bundle
import scitzen.parser.{Attribute, Inline, InlineQuote, InlineText, Macro}
import scitzen.semantics.Sast
import scitzen.semantics.Sast._
import scitzen.semantics.SastAnalyzes.AnalyzeResult

class SastToHtmlConverter[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT],
                                                           bibliography: Map[String, String],
                                                           analyzeResult: AnalyzeResult) {

  import bundle.all._
  import bundle.tags2.aside


  def listItemToHtml(child: SlistItem) = {
    li(
      sastToHtml(child.content),
      sastToHtml(child.inner)
      )
  }

  def sastToHtml(b: Sast): Frag = {
    b match {

      case Sseqf(inner) => SeqFrag(inner.map(sastToHtml))

      case AttributeDef(_) => frag()

      case Text(inner) => inlineValuesToHTML(inner)

      case Section(title, content) =>
        SeqFrag(List(tag("h1")(id := title.str, inlineValuesToHTML(title.inline)),
                     sastToHtml(content)))

      case Slist(children) =>
        val listTag = if (children.head.marker.contains(".")) ol else ul
        listTag(children.map(listItemToHtml))

      case MacroBlock(Macro("image", attributes)) =>
        val target = attributes.last.value
        div(cls := "imageblock",
            img(src := target)
            )
      case MacroBlock(Macro("label", attributes)) =>
        frag()

      case ParsedBlock(delimiter, content) =>
        if (delimiter == "") p(sastToHtml(content))
        else delimiter.charAt(0) match {
          // Sidebar
          // Parsed as a normal document content, but may float off to some side.
          case '*' => aside(sastToHtml(content))
          case '_' => blockquote(sastToHtml(content))
          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case ' ' => sastToHtml(content)
          // there is also '=' example, and '+' passthrough.
          // examples seems rather specific, and passthrough is not implemented.
          case _   => div(delimiter, br, sastToHtml(content), br, delimiter)
        }

      case RawBlock(delimiter, text) =>
        delimiter.charAt(0) match {
          // Code listing
          // Use this for monospace, space preserving, line preserving text
          // It may wrap to fit the screen content
          case '-' | '`' => pre(code(text))
          // Literal block
          // This seems to be supposed to work similar to code? But whats the point then?
          // We interpret this as text with predetermined line wrappings
          // and no special syntax, but otherwise normally formatted.
          // This is great to represent copy&pasted posts or chat messages.
          case '.' => pre(text)
        }

      case other : MacroBlock =>
        scribe.warn(s"not implemented: $other")
        div(stringFrag(other.toString))

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


  def inlineValuesToHTML(inners: Seq[Inline]): Seq[Frag] = inners.map[Frag, Seq[Frag]] {
    case InlineText(str) => str
    case InlineQuote(q, inner) =>
      //scribe.warn(s"inline quote $q: $inner; ${post.sourcePath}")
      q.head match {
      case '_' => em(inner)
      case '*' => strong(inner)
      case '`' => code(inner)
      case '$' => span(raw((scala.sys.process.Process(s"npx katex") #< new ByteArrayInputStream(inner.getBytes(StandardCharsets.UTF_8))).!!))
    }
    case Macro("//", attributes) => frag()
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
    case im @ Macro(command, attributes) =>
      scribe.warn(s"inline macro “$command[$attributes]”")
      code(s"$command[${attributes.mkString(",")}]")
  }
  def linkTo(attributes: Seq[Attribute], linktarget: String) = {
    a(href := linktarget)(attributes.find(_.id == "").fold(linktarget)(_.value))
  }
}
