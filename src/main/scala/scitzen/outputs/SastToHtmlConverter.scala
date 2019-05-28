package scitzen.outputs

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import better.files.File
import kaleidoscope.RegexStringContext
import scalatags.generic.Bundle
import scitzen.generic.ParsedDocument
import scitzen.extern.TexTikz
import scitzen.generic.Sast._
import scitzen.generic.{DocumentManager, ImageResolver, Sast, Sdoc}
import scitzen.parser.{Attributes, Inline, InlineQuote, InlineText, Macro, ScitzenDateTime}

import scala.collection.mutable


object ImportPreproc {
  def macroImportPreproc(docOpt: Option[ParsedDocument], attributes: Attributes)
  : Option[(ParsedDocument, Seq[TLBlock])] = {
    val res = docOpt match {
      case None      =>
        scribe.warn(s"include unknown document ${attributes.target} omitting")
        None
      case Some(doc) =>
        val sast = if (attributes.named.get("format").contains("article")) {
          val date = doc.sdoc.date.fold("")(d => d.date.full + " ")
          val head = doc.blocks.head
          val section = head.content.asInstanceOf[Section]
          val sast = head.copy(content = section.copy(title = Text(InlineText(date) +: section.title.inline)))
          List(sast)
        } else doc.blocks
        Some(doc -> sast)
    }
    res
  }
}


class SastToHtmlConverter[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT],
                                                           documentManager: DocumentManager,
                                                           imageResolver: ImageResolver,
                                                           bibliography: Map[String, String],
                                                           sdoc: Sdoc,
                                                           ownpath: File,
                                                           katexMap: mutable.Map[String, String],
                                                           sync: Option[(File, Int)]) {

  import bundle.all._
  import bundle.tags2.article

  val root = ownpath.parent

  val syncPos = {
    if (sync.exists(_._1 == ownpath)) sync.get._2 else Int.MaxValue
  }

  def convert() = cBlocks(sdoc.blocks)


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

  def cBlocks(blocks: Seq[TLBlock])(implicit nestingLevel: Scope = new Scope(1)): Seq[Frag] = {
    blocks.flatMap { bwa: TLBlock =>
      val positiontype = bwa.attr.positional.headOption
      positiontype match {
        case Some("image") =>
          val (hash, _) = TexTikz.getHash(bwa.content.asInstanceOf[RawBlock].content)
          val path = imageResolver.image(hash)
          List(img(src := path))
        case Some("quote") =>
          val innerHtml = sastToHtml(List(bwa.content))
          // for blockquote layout, see example 12 (the twitter quote)
          // http://w3c.github.io/html/textlevel-semantics.html#the-cite-element
          val bq = blockquote(innerHtml)
          // first argument is "quote" we concat the rest and treat them as a single entity
          val title = bwa.attr.positional.drop(1)
          List(if (title.nonEmpty) bq(cite(title))
               else bq)
        case _             =>
          val prov = bwa.prov
          val html = sastToHtml(List(bwa.content))
          if (prov.start <= syncPos && syncPos <= prov.end) {
            scribe.info(s"highlighting $syncPos: $prov")
            html.toList match {
              case h :: tail =>
                div(id := "highlight", h) :: tail
              case Nil            => List(div(id := "highlight"))
            }
          } else html
      }
    }
  }

  def sastToHtml(b: Seq[Sast])(implicit nestingLevel: Scope = new Scope(1)): Seq[Frag] = {
    b.flatMap[Frag, Seq[Frag]] {

      case AttributeDef(_) => Nil

      case sec@Section(title, subsections) =>
        val inner = (if (nestingLevel.level == 1) List(tMeta()) else Nil) ++
                     cBlocks(subsections)(nestingLevel.inc)
        tag("h" + nestingLevel.level)(id := title.str, inlineValuesToHTML(title.inline)) +:
        inner

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
          val target = imageResolver.image(root, attributes.positional.last)
          List(img(src := target))
        case Macro("label", attributes) => Nil
        case Macro("horizontal-rule", attributes) => List(hr)
        case Macro("include", attributes) if attributes.named.get("type").contains("article") =>
          val post = documentManager.find(root, attributes.target).get

          def timeShort(date: ScitzenDateTime) = {
            span(cls := "time", stringFrag(date.monthDayTime + " "))
          }

          def categoriesSpan() = {
            span(cls := "category")(post.sdoc.named.get("categories"), post.sdoc.named.get("people"))
          }

         val aref = documentManager.relTargetPath(root, post)

          List(a(
            href := aref,
            article(timeShort(post.sdoc.date.get),
                    span(cls := "title", post.sdoc.title),
                    categoriesSpan()
            )))

        case Macro("include", attributes) =>
          ImportPreproc.macroImportPreproc(documentManager.find(root, attributes.target), attributes) match {
            case Some((doc, sast)) =>
              new SastToHtmlConverter(bundle, documentManager, imageResolver,
                                      bibliography, doc.sdoc, doc.file, katexMap, sync)
              .cBlocks(sast)(new Scope(3))
            case None => Nil
          }



        case other =>
          scribe.warn(s"not implemented: $other")
          List(div(stringFrag(other.toString)))
      }

      case Paragraph(content) => List(p(inlineValuesToHTML(content.inline)))

      case ParsedBlock(delimiter, blockContent) =>
        delimiter match {
          case r"=+" => List(figure(cBlocks(blockContent)))
          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case r"\s+" => cBlocks(blockContent)
          // includes are also included as is
          case "include" => cBlocks(blockContent)
          // there is also '=' example, and '+' passthrough.
          // examples seems rather specific, and passthrough is not implemented.
          case _ => List(div(delimiter, br, cBlocks(blockContent), br, delimiter))
        }

      case RawBlock(delimiter, text) =>
        if (delimiter.isEmpty) Nil
        else delimiter.charAt(0) match {
          // Code listing
          // Use this for monospace, space preserving, line preserving text
          // It may wrap to fit the screen content
          case '`' => List(pre(code(text)))
          // Literal block
          // This seems to be supposed to work similar to code? But whats the point then?
          // We interpret this as text with predetermined line wrappings
          // and no special syntax, but otherwise normally formatted.
          // This is great to represent copy&pasted posts or chat messages.
          case '.' => List(pre(text))
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
