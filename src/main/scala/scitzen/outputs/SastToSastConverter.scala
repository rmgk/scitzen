package scitzen.outputs

import better.files.File
import cats.data.Chain
import scitzen.generic.Sast._
import scitzen.generic.{ConversionContext, PDReporter, Project, Reporter, Sast, Scope}
import scitzen.parser.MacroCommand.{Cite, Comment, Def, Image, Include, Label, Link, Other, Quote, Ref}
import scitzen.parser.{Inline, InlineText, Macro}


class SastToSastConverter(project: Project,
                          cwd: File,
                          reporter: Reporter
                         ) {

  type CtxCS = ConversionContext[Chain[Sast]]
  type Ctx[T] = ConversionContext[T]
  type Cta = Ctx[_]


  def convertSeq(b: Seq[Sast])(implicit ctx: Cta): CtxCS = {
    ctx.fold(b) { (ctx, sast) => convertSingle(sast)(ctx) }
  }

  def convertSingle(sast: Sast)(implicit ctx: Cta): CtxCS = sast match {

      case tlBlock: TLBlock => convertBlock(tlBlock)

      case Section(title, contents, attr) =>
        val conCtx = convertSeq(contents)(ctx)
        convertInlines(title.inline)(conCtx).map{ title =>
          Section(Text(title, conCtx.data, attr))
        }


      case Slist(children) =>
        ctx.fold(children){ case (ctx, child) =>
          convertSeq(child.content)(ctx).map(con => Chain(SlistItem(child.marker, con.iterator.toSeq)))
        }.map {cs =>
          Chain(Slist(cs.iterator.toSeq))
        }

      case MacroBlock(mcro) => mcro match {
        case Macro(Image, attributes) =>
          val target    = attributes.target
          attributes.named.get("converter") match {
            case Some(converter) =>
              convertSeq(ctx.converter.convert(cwd, mcro, "pdf"))

            case None =>
              ctx.project.resolve(cwd, target) match {
                case None       =>
                  scribe.error(s"Not relative path: $mcro")
                  ctx.empty
                case Some(data) =>
                  ctx.ret(Chain(s"\\noindent{}\\includegraphics[width=\\columnwidth]{$data}\n"))
              }
          }


        case Macro(Include, attributes) =>
          ImportPreproc.macroImportPreproc(project.findDoc(cwd, attributes.target), attributes) match {
            case Some((doc, sast)) =>
              ctx.withScope(new Scope(3))(
                new SastToSastConverter(project, doc.parsed.file.parent, new PDReporter(doc.parsed))
                .convertSeq(sast)(_))

            case None => ctx.empty
          }

        case other =>
          convertInlines(List(other)).single
      }
  }

  def convertBlock(tlblock: TLBlock)(implicit ctx: Cta): CtxCS = tlblock.content match {
      case Paragraph(content) => convertInlines(content.inline).single :+ ""


      case ParsedBlock(delimiter, blockContent) =>
        delimiter.charAt(0) match {
          case '=' =>
            tlblock.attr.positional.headOption match {
              case Some(blockname) =>
                blockname match {
                  case "figure" =>
                    val (figContent, caption) = {
                      blockContent.lastOption match {
                        case Some(inner @ TLBlock(_, Paragraph(content))) =>
                          val captionstr = convertInlines(content.inline).single.data.iterator.mkString("\n")
                          (blockContent.init,
                          s"\\caption{$captionstr}")
                        case other                                        =>
                          scribe.warn(s"figure has no caption")
                          (blockContent, "")
                      }
                    }
                    "\\begin{figure}" +:
                    convertSeq(figContent) :++
                    Chain(
                      caption,
                      tlblock.attr.named.get("label").fold("")(l => s"\\label{$l}"),
                      "\\end{figure}"
                      )

                  case name@ ("theorem"|"definition"|"proofbox"|"proof"|"lemma"|"example") =>

                    texbox(name, tlblock.attr.positional.tail, blockContent)


                  case other =>
                    convertSeq(blockContent)
                }

              case None =>
                convertSeq(blockContent)
            }

          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case ' ' => convertSeq(blockContent)
          // there is also '=' example, and '+' passthrough.
          // examples seems rather specific, and passthrough is not implemented.
          case _   => convertSeq(blockContent)
        }

      case RawBlock(delimiter, text) =>
        if (tlblock.attr.named.contains("converter"))
          convertSeq(ctx.converter.convert(tlblock, "pdf"))
        else if (delimiter.isEmpty || delimiter == "comment|space") ctx.empty
        else delimiter.charAt(0) match {
          case '`' =>
            val restext = tlblock.attr.named.get("label") match {
              case None => text
              case Some(label) =>
                text.replaceAll(""":§([^§]*?)§""", s"""(*@\\\\label{$label$$1}@*)""")
            }
            ctx.ret(Chain(s"\\begin{lstlisting}", restext, "\\end{lstlisting}"))
          case '.' =>
            val latexenc = latexencode(text).trim
                           .replaceAll("\n{2,}", """\\newline{}\\noindent{}""")
                            .replaceAllLiterally("\n", "\\newline{}\n")
            ctx.ret(Chain("\\noindent",latexenc))
        }




    }

  def convertInlines(inners: Seq[Inline])(implicit ctx: Cta): Ctx[String] = ctx.ret(inners.map {
    case InlineText(str) => latexencode(str)

    case Macro(Def, _) => ""

    case Macro(Quote(q), inner2) =>
      val inner = latexencode(inner2.target)
      //scribe.warn(s"inline quote $q: $inner; ${post.sourcePath}")
      q.head match {
        case '_' => s"\\emph{$inner}"
        case '*' => s"\\textbf{$inner}"
        case '`' => s"\\texttt{$inner}"
        case '$' => s"$$${inner2.target}$$"
      }
    case Macro(Comment, attributes) => ""

    case Macro(Ref, attributes)      => s"\\ref{${attributes.target}}"

    case Macro(Cite, attributes)              =>
      s"\\cite{${attributes.target}}"

    case Macro(Link, attributes)              =>
      val target = attributes.target
      if (attributes.positional.size > 1) {
        val name = """{"""" + attributes.positional.head + """"}"""
        s"\\href{$target}{$name}"
      }
      else s"\\url{$target}"

    case Macro(Other("n"), attributes) if project.documentManager.attributes.contains(attributes.target) =>
      project.documentManager.attributes(attributes.target)

    case Macro(Other("footnote"), attributes) =>
      val target = latexencode(attributes.target)
      s"\\footnote{$target}"

    case Macro(Label, attributes)             => s"\\label{${attributes.target}}"

    case Macro(Other("tableofcontents"), attributes) =>
      List("\\clearpage", "\\tableofcontents*", "\\clearpage").mkString("\n")

    case Macro(Other("subparagraph"), attributes) =>
      s"\\subparagraph{${attributes.target}}"

    case Macro(Other("textsc"), attributes) =>
      s"\\textsc{${attributes.target}}"

    case im @ Macro(command, attributes)      =>
      scribe.warn(s"inline macro “$command[$attributes]”")
      s"$command[${attributes.all.mkString(",")}]"
  }.mkString(""))

  def reportPos(m: Macro): String = reporter(m)

}
