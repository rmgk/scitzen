package scitzen.outputs

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import better.files.File
import kaleidoscope.RegexStringContext
import scalatags.generic.Bundle
import scitzen.extern.Tex
import scitzen.generic.Sast._
import scitzen.generic.{DocumentManager, Sast, Sdoc}
import scitzen.parser.{Attribute, Attributes, Inline, InlineQuote, InlineText, Macro, ScitzenDateTime}

import scala.collection.mutable
import scala.util.Try



class SastToHtmlConverter[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT],
                                                           documentManager: DocumentManager,
                                                           bibliography: Map[String, String],
                                                           sdoc: Sdoc,
                                                           root: File,
                                                           katexMap: mutable.Map[String, String]) {

  import bundle.all._
  import bundle.tags2.article

  def convert() = sastToHtml(sdoc.sast)


  def listItemToHtml(child: SlistItem)(implicit nestingLevel: Scope) = {
      sastToHtml(child.content)
  }

  def tMeta() = {

    def timeFull(date: ScitzenDateTime): Tag = {
      //need time formatter, because to string removes seconds if all zero
      span(cls := "time", date.full)
    }

    def categoriesSpan() = {
      val categories = List("categories", "people").flatMap(sdoc.named.get)
                       .flatMap(_.split(","))
      span(cls := "category")(categories.map(c => stringFrag(s" $c ")): _*)
    }

    p(cls := "metadata",
      sdoc.date.map(timeFull).getOrElse(""),
      frag(sdoc.modified.map(timeFull).toList: _*),
      categoriesSpan(),
      frag(sdoc.named.get("folder").map(f => span(cls := "category")(stringFrag(s" in $f"))).toList: _*)
      )
  }

  def sastToHtml(b: Seq[Sast])(implicit nestingLevel: Scope = new Scope(1)): Seq[Frag] = {
    b.flatMap[Frag, Seq[Frag]] {

      case AttributeDef(_) => Nil

      case Text(inner) => inlineValuesToHTML(inner)

      case sec@Section(title, subsections) =>
        List(tag("h" + nestingLevel.level)(id := title.str, inlineValuesToHTML(title.inline)),
             if (nestingLevel.level == 1) tMeta() else frag(),
             sastToHtml(subsections)(nestingLevel.inc))

      case Slist(Nil) => Nil
      case Slist(children) => List(
        if (children.head.marker.contains(":")) {
          dl(children.flatMap(c => List(dt(strong(c.marker.replaceAllLiterally(":", ""))), dd(listItemToHtml(c)))))
        }
        else {
          val listTag = if (children.head.marker.contains(".")) ol else ul
          listTag(children.map(c => li(listItemToHtml(c))))
        })

      case MacroBlock(mcro) => mcro match {
        case Macro("image", attributes) =>
          val target = attributes.positional.last
          List(div(cls := "imageblock",
              img(src := target)))
        case Macro("label", attributes) => Nil
        case Macro("horizontal-rule", attributes) => List(hr)
        case Macro("include", attributes) if attributes.named.get("type").contains("article") =>
          val post = documentManager.find(root, attributes.target).get

          def timeShort(date: ScitzenDateTime) = {
            span(cls := "time",
                 stringFrag(
                   date.monthDayTime))
          }

          def categoriesSpan() = {
            span(cls := "category")(post.sdoc.named.get("categories"), post.sdoc.named.get("people"))
          }

         val aref = documentManager.relTargetPath(root, post)

          List(a( cls:= "articleref",
            href := aref,
            article(timeShort(post.sdoc.date.get),
                    span(cls := "title", post.sdoc.title),
                    categoriesSpan()
            )))


        case other =>
          scribe.warn(s"not implemented: $other")
          List(div(stringFrag(other.toString)))
      }

      case ParsedBlock(delimiter, blockContent) =>
        List(
        delimiter match {
          case "" => p(sastToHtml(blockContent))
          case r"=+" => figure(sastToHtml(blockContent))
          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case r"\s+" => sastToHtml(blockContent)
          // includes are also included as is
          case "include" => sastToHtml(blockContent)
          // there is also '=' example, and '+' passthrough.
          // examples seems rather specific, and passthrough is not implemented.
          case _ => div(delimiter, br, sastToHtml(blockContent), br, delimiter)
        })

      case RawBlock(delimiter, text) => List(
        if (delimiter.isEmpty) stringFrag("")
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
        })



      case bwa: AttributedBlock => List {
        val positiontype = bwa.attr.attributes.positional.headOption
        positiontype match {
          case Some("image") =>
            val target = File("tempdir")
            val pdf = Tex.convert(bwa.content.asInstanceOf[RawBlock].content, target)
            val svg = Tex.pdfToSvg(pdf)
            sastToHtml(List(MacroBlock(Macro("image", List(Attribute("", svg.pathAsString))))))
          case Some("quote")                                                          =>
            val innerHtml = sastToHtml(List(bwa.content))
            // for blockquote layout, see example 12 (the twitter quote)
            // http://w3c.github.io/html/textlevel-semantics.html#the-cite-element
            val bq = blockquote(innerHtml)
            // first argument is "quote" we concat the rest and treat them as a single entity
            val title = bwa.attr.attributes.positional.drop(1)
            if (title.nonEmpty) bq(cite(title))
            else bq
          case _             => sastToHtml(List(bwa.content))
        }
      }
    }
  }




  def tableOfContents(): Option[Frag] = {
    def findSections(cont: Seq[Sast]): Seq[Section] = {
      cont.flatMap {
        case s: Section => List(s)
        case AttributedBlock(_, content) => findSections(List(content))
        case ParsedBlock(_, content) => findSections(content)
        case _ => Nil
      }
    }
    def makeToc(cont: Seq[Sast], depth: Int): Tag = {
      ol(findSections(cont).map {
        case Section(title, inner) =>
          val sub = if (depth > 1) makeToc(inner, depth - 1) else frag()
          li(a(href := s"#${title.str}", title.str))(sub)
      })
    }

    sdoc.named.get("toc") map { depth =>
      val d = Try {depth.trim.toInt}.getOrElse(1)
      sdoc.sast match {
        case Seq(Section(_, secCon)) => makeToc(secCon, d)
        case other                   => makeToc(other, d)
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
      case '$' =>
        val mathml = katexMap.getOrElseUpdate(inner, {
          (scala.sys.process.Process(s"katex") #< new ByteArrayInputStream(inner.getBytes(StandardCharsets.UTF_8))).!!
        })
        span(raw(mathml))
    }
    case Macro("comment", attributes) => frag()
    case Macro("ref", attributes) =>
      sdoc.targets.find(_.id == attributes.positional.head).map { target =>
        target.resolution match {
          case Section(title, _) => a(href := s"#${title.str}", inlineValuesToHTML(title.inline))
          case other =>
            scribe.error(s"can not refer to $other")
            frag()
        }
      }
    case Macro("cite", attributes) =>
      val anchors = attributes.positional.flatMap{_.split(",")}.map{ bibid =>
        bibid -> bibliography.getOrElse(bibid.trim, {
          scribe.error(s"bib key not found: $bibid")
          bibid
        })
      }.sortBy(_._2).map{case (bibid, bib) => a(href := s"#$bibid", bib)}
      frag("\u00A0", anchors)
    case Macro(tagname@("ins" | "del"), attributes) =>
      tag(tagname)(attributes.positional.mkString(", "))
    case Macro(protocol @ ("http" | "https" | "ftp" | "irc" | "mailto"), attributes) =>
      val linktarget = s"$protocol:${attributes.target}"
      linkTo(attributes, linktarget)
    case Macro("link", attributes) =>
      val target = attributes.target
      linkTo(attributes, target)
    case Macro("footnote", attributes) =>
      val target = attributes.target
      a(title := target, "※")
    case im @ Macro(command, attributes) =>
      scribe.warn(s"inline macro “$command[$attributes]”")
      code(s"$command[${attributes.all.mkString(",")}]")
  }
  def linkTo(attributes: Attributes, linktarget: String) = {
    a(href := linktarget)(attributes.positional.headOption.getOrElse[String](linktarget))
  }
}
