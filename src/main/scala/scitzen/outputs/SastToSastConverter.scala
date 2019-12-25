package scitzen.outputs

import better.files.File
import cats.data.Chain
import scitzen.generic.Sast._
import scitzen.generic.{ConversionContext, PDReporter, Project, Reporter, Sast}
import scitzen.parser.MacroCommand.{Image, Include}
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
      convertInlines(title.inline)(conCtx).map { title =>
        Chain(Section(Text(title.toList), conCtx.data.toList, attr))
      }


    case Slist(children) =>
      ctx.fold[SlistItem, SlistItem](children) { (ctx, child) =>
        convertSeq(child.content)(ctx).map(con => Chain(SlistItem(child.marker, con.iterator.toSeq)))
      }.map { cs =>
        Chain(Slist(cs.iterator.toSeq))
      }

    case MacroBlock(mcro) => mcro match {
      case Macro(Image, attributes) if attributes.named.contains("converter") =>
        convertSeq(ctx.converter.convert(cwd, mcro))


      case Macro(Include, attributes) =>
        val included = project.findDoc(cwd, attributes.target)
        ImportPreproc.macroImportPreproc(included, attributes) match {
          case Some((doc, sast)) =>
            new SastToSastConverter(project, doc.parsed.file.parent, new PDReporter(doc.parsed))
            .convertSeq(sast)

          case None => ctx.empty
        }

      case other =>
        convertMacro(other).map(MacroBlock(_): Sast).single
    }
  }

  def convertBlock(tlblock: TLBlock)(implicit ctx: Cta): CtxCS = tlblock.content match {
    case Paragraph(content) =>
      convertInlines(content.inline)
      .map(il => TLBlock(tlblock.attr, Paragraph(Text(il.toList))): Sast).single


    case ParsedBlock(delimiter, blockContent) =>
      convertSeq(blockContent).map(bc => TLBlock(tlblock.attr, ParsedBlock(delimiter, bc.toList)): Sast).single

    case RawBlock(delimiter, text) =>
      if (tlblock.attr.named.contains("converter"))
        convertSeq(ctx.converter.convert(tlblock))
      else if (delimiter.isEmpty || delimiter == "comment|space") ctx.empty
      else {
        ctx.retc(TLBlock(tlblock.attr, RawBlock(delimiter, text)))
      }


  }

  def convertInlines(inners: Seq[Inline])(implicit ctx: Cta): Ctx[Chain[Inline]] =
    ctx.fold(inners) { (ctx, inline) =>
      inline match {
        case inlineText: InlineText => ctx.retc(inlineText)
        case m: Macro               => convertMacro(m)(ctx).map(identity[Inline]).single
      }
    }

  def convertMacro(inners: Macro)(implicit ctx: Cta): Ctx[Macro] = ctx.ret(inners)

}
