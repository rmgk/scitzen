package scitzen.outputs

import better.files.File
import cats.data.Chain
import scitzen.generic.Sast._
import scitzen.generic.{ConversionContext, ImageConverter, Project, Reporter, Sast, SastRef}
import scitzen.parser.MacroCommand.{Image, Include, Label}
import scitzen.parser.{Attribute, Inline, InlineText, Macro}


class SastToSastConverter(project: Project,
                          cwf: File,
                          reporter: Reporter,
                          converter: ImageConverter
                         ) {

  type CtxCS = ConversionContext[Chain[Sast]]
  type Ctx[T] = ConversionContext[T]
  type Cta = Ctx[_]

  val cwd = if (cwf.isDirectory) cwf else cwf.parent


  def convertSeq(b: Seq[Sast])(implicit ctx: Cta): CtxCS = {
    ctx.fold(b) { (ctx, sast) => convertSingle(sast)(ctx) }
  }

  def convertSingle(sast: Sast)(implicit ctx: Cta): CtxCS = sast match {

    case tlBlock: SBlock => convertBlock(tlBlock)

    case sec @ Section(title, contents, attr) =>

      val ref1 = sec.ref

      val nextCtx =
        if (ctx.labelledSections.contains(ref1)) {
          val ctr    = ctx.nextId
          val cp     = sec.copy(attributes = attr.updated("label", s"$ref1 (${ctr.data})"))
          val secref = SastRef(cwf, cp)
          ctr.addSection(ref1, secref).addSection(cp.ref, secref)
        } else {
          ctx.addSection(ref1, SastRef(cwf, sec))
        }

      val conCtx = convertSeq(contents)(nextCtx.push(nextCtx.data.sast)).pop()
      convertInlines(title.inline)(conCtx).map { title =>
        Chain(Section(Text(title.toList), conCtx.data.toList, nextCtx.data.sast.attributes))
      }


    case Slist(children) =>
      ctx.fold[SlistItem, SlistItem](children) { (ctx, child) =>
        convertSeq(child.content)(ctx).map(con => Chain(SlistItem(child.marker, con.iterator.toSeq)))
      }.map { cs =>
        Chain(Slist(cs.iterator.toSeq))
      }

    case SMacro(mcro) =>
      convertMacro(mcro).map(SMacro(_): Sast).single
  }

  def convertBlock(tlblock: SBlock)(implicit ctx: Cta): CtxCS = tlblock.content match {
    case Paragraph(content) =>
      convertInlines(content.inline)
      .map(il => SBlock(tlblock.attributes, Paragraph(Text(il.toList))): Sast).single


    case Parsed(delimiter, blockContent) =>
      convertSeq(blockContent).map(bc => SBlock(tlblock.attributes, Parsed(delimiter, bc.toList)): Sast).single

    case Fenced(text) =>
      if (tlblock.attributes.named.contains("converter")) {
        val resctx = converter.convert(tlblock).schedule(ctx)
        convertSingle(resctx.data)(resctx)
      }
      else {
        ctx.retc(tlblock)
      }

    case SpaceComment(content) => ctx.retc(tlblock)


  }

  def convertInlines(inners: Seq[Inline])(implicit ctx: Cta): Ctx[Chain[Inline]] =
    ctx.fold(inners) { (ctx, inline) =>
      inline match {
        case inlineText: InlineText => ctx.retc(inlineText)
        case m: Macro               => convertMacro(m)(ctx).map(identity[Inline]).single
      }
    }

  def convertMacro(mcro: Macro)(implicit ctx: Cta): Ctx[Macro] = mcro match {
    case Macro(Image, attributes) if attributes.named.contains("converter") =>
      val resctx = converter.convert(cwd, mcro).schedule(ctx)
      convertMacro(resctx.data)(resctx)

    case Macro(Image, attributes) if attributes.target.endsWith("pdf") && converter.formatHint == "svg" =>
      project.resolve(cwd, attributes.target).fold(ctx.ret(mcro)) { file =>
        val resctx    = converter.pdftosvg(file).schedule(ctx)
        val reltarget = cwd.relativize(resctx.data)
        convertMacro(Macro(Image, attributes.copy(
          raw = attributes.raw.init :+ Attribute("", reltarget.toString))))(resctx)
      }

    case Macro(Label, attributes) =>
      ctx.addSection(attributes.target, SastRef(cwf, ctx.stack.head)).ret(mcro)

    case Macro(Include, attributes) =>
      project.resolve(cwd, attributes.target) match {
        case None =>
          scribe.error(s"unknown include ${attributes.target}" + reporter(mcro))
          ctx.ret(mcro)
        case Some(file) => ctx.copy(includes = file :: ctx.includes).ret(mcro)
      }


    case other => ctx.ret(other)

  }

}
