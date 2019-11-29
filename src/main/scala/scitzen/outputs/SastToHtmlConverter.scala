package scitzen.outputs

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import better.files.File
import scalatags.generic.Bundle
import scitzen.extern.Hashes
import scitzen.generic.Sast._
import scitzen.generic.{DocumentManager, ExternalContentResolver, ParsedDocument, Project, Sast, Sdoc}
import scitzen.parser.MacroCommand.{Cite, Comment, Image, Include, Link, Other, Quote}
import scitzen.parser.{Attributes, Inline, InlineText, Macro, ScitzenDateTime}

import scala.collection.mutable

import scitzen.generic.RegexContext.regexStringContext


object ImportPreproc {
  def macroImportPreproc(docOpt: Option[ParsedDocument], attributes: Attributes)
  : Option[(ParsedDocument, Seq[Sast])] = {
    val res = docOpt match {
      case None      =>
        scribe.warn(s"include unknown document ${attributes.target} omitting")
        None
      case Some(doc) =>
        val sast = if (attributes.named.get("format").contains("article")) {
          val date = doc.sdoc.date.fold("")(d => d.date.full + " ")
          val head = doc.sdoc.blocks.head
          val section = head.asInstanceOf[Section]
          val sast = section.copy(title = Text(InlineText(date) +: section.title.inline))
          List(sast)
        } else doc.sdoc.blocks
        Some(doc -> sast)
    }
    res
  }
}


class SastToHtmlConverter[Builder, Output <: FragT, FragT]
(val bundle: Bundle[Builder, Output, FragT],
 documentManager: DocumentManager,
 imageResolver: ExternalContentResolver,
 bibliography: Map[String, String],
 sdoc: Sdoc,
 document: Option[ParsedDocument],
 katexMap: mutable.Map[String, String],
 sync: Option[(File, Int)],
 project: Project) {

  import bundle.all._
  import bundle.tags2.{article, time}

  val currentFile: File = document.fold(project.root)(_.file)

  val syncPos =
    document.filter(doc => sync.exists(_._1 == doc.file))
    .fold(Int.MaxValue){_ => sync.get._2}

  def convert() = cBlocks(sdoc.blocks)


  def listItemToHtml(child: SlistItem)(implicit nestingLevel: Scope): Seq[Frag] = {
      sastToHtml(child.content)
  }

  def tMeta() = {

    def timeFull(date: ScitzenDateTime): Tag = time(date.full)

    def categoriesSpan() = {
      val categories = List("categories", "people").flatMap(sdoc.named.get)
                       .flatMap(_.split(","))
      span(cls := "category")(categories.map(c => stringFrag(s" $c ")): _*)
    }

    div(cls := "metadata",
      sdoc.date.map(timeFull).getOrElse(""),
      frag(sdoc.modified.map(timeFull).toList: _*),
      categoriesSpan(),
      frag(sdoc.named.get("folder").map(f => span(cls := "category")(stringFrag(s" in $f"))).toList: _*)
      )
  }

  def cBlocks(blocks: Seq[Sast])(implicit nestingLevel: Scope = new Scope(1)): Seq[Frag] = sastToHtml(blocks)

  def tlBlockToHtml(bwa: TLBlock)(implicit nestingLevel: Scope = new Scope(1)): Seq[Frag] = {
      val positiontype = bwa.attr.positional.headOption
      positiontype match {
        case Some("image") =>
          val hash = Hashes.sha1hex(bwa.content.asInstanceOf[RawBlock].content)
          val path = imageResolver.image(hash)
          List(img(src := path))
        case Some("quote") =>
          val innerHtml = sblockToHtml(bwa.content)
          // for blockquote layout, see example 12 (the twitter quote)
          // http://w3c.github.io/html/textlevel-semantics.html#the-cite-element
          val bq = blockquote(innerHtml)
          // first argument is "quote" we concat the rest and treat them as a single entity
          val title = bwa.attr.positional.drop(1)
          List(if (title.nonEmpty) bq(cite(title))
               else bq)
        case _             =>
          val prov = bwa.attr.prov
          val html = sblockToHtml(bwa.content)
          if (prov.start <= syncPos && syncPos <= prov.end) {
            scribe.info(s"highlighting $syncPos: $prov")
            div(id := "highlight") :: html.toList
          } else html
      }
  }

  def sastToHtml(b: Seq[Sast])(implicit nestingLevel: Scope = new Scope(1)): Seq[Frag]  = b.flatMap(sastToHtmlI(_))
  def sastToHtmlI(b: Sast)(implicit nestingLevel: Scope = new Scope(1)): Seq[Frag] = {
    b match {

      case tlblock: TLBlock => tlBlockToHtml(tlblock)

      case sec@Section(title, subsections, _) =>
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
        case Macro(Image, attributes) =>
          val target = imageResolver.image(currentFile, attributes.positional.last)
          List(img(src := target))
        case Macro(Other("horizontal-rule"), attributes) => List(hr)
        case Macro(Include, attributes) if attributes.named.get("type").contains("article") =>
          val post = project.findDoc(currentFile, attributes.target).get

          def timeShort(date: ScitzenDateTime) = {
            time(stringFrag(date.monthDayTime + " "))
          }

          def categoriesSpan() = {
            span(cls := "category")(post.sdoc.named.get("categories"), post.sdoc.named.get("people"))
          }

         val aref = documentManager.relTargetPath(currentFile, post)

          List(a(
            href := aref,
            article(timeShort(post.sdoc.date.get),
                    span(cls := "title", post.sdoc.title),
                    categoriesSpan()
            )))

        case Macro(Include, attributes) =>
          ImportPreproc.macroImportPreproc(project.findDoc(currentFile, attributes.target), attributes) match {
            case Some((doc, sast)) =>
              new SastToHtmlConverter(bundle, documentManager, imageResolver,
                                      bibliography, doc.sdoc, Some(doc), katexMap, sync, project)
              .cBlocks(sast)(new Scope(3))
            case None => Nil
          }



        case other=>
          inlineValuesToHTML(List(other))
        }
      }
    }

    def sblockToHtml(sblockType: SBlockType)(implicit nestingLevel: Scope = new Scope(1)): Seq[Frag] = sblockType match {

      case Paragraph(text) => List(p(inlineValuesToHTML(text.inline)))

      case ParsedBlock(delimiter, blockContent) =>
        delimiter match {
          case rex"=+" => List(figure(cBlocks(blockContent)))
          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case rex"\s+" => cBlocks(blockContent)
          // includes are also included as is
          case "include" => cBlocks(blockContent)
          // there is also '=' example, and '+' passthrough.
          // examples seems rather specific, and passthrough is not implemented.
          case _ => List(div(delimiter, br, cBlocks(blockContent), br, delimiter))
        }

      case RawBlock(delimiter, text) =>
        if (delimiter.isEmpty || delimiter == "comment|space") Nil
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
          case other =>
            scribe.warn(s"unknown block type “$delimiter”")
            List(pre(code(text)))
        }
    }


  def inlineValuesToHTML(inlines: Seq[Inline]): Seq[Frag] =
    inlines.map {
      case InlineText(str) => str
      case Macro(Quote(q), attrs) =>
        val inner = attrs.target
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
      case Macro(Comment, attributes) => frag()
      case Macro(Other("ref"), attributes) =>
        sdoc.targets.find(_.id == attributes.positional.head).map { target =>
          target.resolution match {
            case Section(title, _, _) => a(href := s"#${title.str}", inlineValuesToHTML(title.inline))
            case other =>
              scribe.error(s"can not refer to $other")
              frag()
          }
        }
      case mcro@Macro(Cite, attributes) =>
        val anchors = attributes.positional.flatMap{_.split(",")}.map{ bibid =>
          bibid -> bibliography.getOrElse(bibid.trim, {
            scribe.error(s"bib key not found: $bibid " + reportPos(mcro))
            bibid
          })
        }.sortBy(_._2).map{case (bibid, bib) => a(href := s"#$bibid", bib)}
        frag("\u00A0", anchors)
      case Macro(Other(tagname@("ins" | "del")), attributes) =>
        tag(tagname)(attributes.positional.mkString(", "))
      case Macro(Other(protocol @ ("http" | "https" | "ftp" | "irc" | "mailto")), attributes) =>
        val linktarget = s"$protocol:${attributes.target}"
        linkTo(attributes, linktarget)
      case Macro(Link, attributes) =>
        val target = attributes.target
        linkTo(attributes, target)
      case Macro(Other("footnote"), attributes) =>
        val target = attributes.target
        a(title := target, "※")
      case im @ Macro(command, attributes) => unknownMacroOutput(im)

  }

  def reportPos(m: Macro): String = document.fold("")(_.reporter(m))

  def unknownMacroOutput(im: Macro): Tag = {
    val str = SastToScimConverter().macroToScim(im)
    scribe.warn(s"unknown macro “$str”" + reportPos(im))
    code(str)
  }

  def linkTo(attributes: Attributes, linktarget: String) = {
    a(href := linktarget)(attributes.positional.headOption.getOrElse[String](linktarget))
  }
}
