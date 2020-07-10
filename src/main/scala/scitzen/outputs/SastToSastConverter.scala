package scitzen.outputs

import better.files.File
import cats.data.Chain
import scitzen.extern.ImageConverter
import scitzen.parser.Sast._
import scitzen.generic.{ConversionContext, Project, Reporter, SastRef, _}
import scitzen.parser.MacroCommand.{Image, Include, Label}
import scitzen.parser.{Attribute, Inline, InlineText, Macro, Sast}

object SastToSastConverter {
  def preprocessRecursive(
      doc: FullDoc,
      ctx: ConversionContext[Map[File, List[Sast]]],
      documentManager: DocumentManager,
      conversionPreproc: ParsedDocument => SastToSastConverter
  ): ConversionContext[Map[File, List[Sast]]] = {
    val acc = ctx.data
    if (acc.contains(doc.parsed.file)) return ctx.ret(acc)
    val preprocessed = conversionPreproc(doc.parsed).convertSeq(doc.sast)(ctx)
    val newctx       = preprocessed.copy(includes = Nil).ret(acc.updated(doc.parsed.file, preprocessed.data.toList))
    preprocessed.includes
      .filter(f => !acc.contains(f))
      .map(f => documentManager.byPath(f))
      .foldLeft(newctx) { (ctx, fd) =>
        preprocessRecursive(fd, ctx, documentManager, conversionPreproc)
      }
  }
}

class SastToSastConverter(project: Project, cwf: File, reporter: Reporter, converter: ImageConverter) {

  type CtxCS  = ConversionContext[Chain[Sast]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[_]

  val cwd = if (cwf.isDirectory) cwf else cwf.parent

  def convertSeq(b: Seq[Sast])(implicit ctx: Cta): CtxCS = {
    ctx.fold(b) { (ctx, sast) => convertSingle(sast)(ctx) }
  }

  def convertSingle(sast: Sast)(implicit ctx: Cta): CtxCS =
    sast match {

      case NoContent => ctx.empty

      case tlBlock: SBlock => convertBlock(tlBlock)(ctx)

      case sec @ Section(title, level, _) =>
        val ctxWithRef = addRefTargetMakeUnique(ctx, sec)
        val conCtx     = ctxWithRef.push(ctxWithRef.data.sast)
        convertInlines(title.inline)(conCtx).map { title =>
          Chain(Section(Text(title.toList), level, ctxWithRef.data.sast.attributes))
        }

      case Slist(children) =>
        ctx.fold[SlistItem, SlistItem](children) { (ctx, child) =>
          convertSingle(child.content)(ctx).map { con =>
            if (con.size > 1) throw new IllegalStateException("list contained more that one child")
            Chain(SlistItem(child.marker, child.text, con.headOption.getOrElse(NoContent)))
          }
        }.map { cs =>
          Chain(Slist(cs.iterator.toSeq))
        }

      case SMacro(mcro) =>
        convertMacro(mcro).map(SMacro(_): Sast).single
    }

  def addRefTargetMakeUnique(ctx: Cta, sec: Section): Ctx[SastRef] = {
    val ref1 = sec.ref
    val attr = sec.attributes
    if (ctx.labelledThings.contains(ref1)) {
      val ctr    = ctx.nextId
      val cp     = sec.copy(attributes = attr.updated("label", s"$ref1 (${ctr.data})"))
      val secref = SastRef(cwf, cp)
      ctr.addRefTarget(ref1, secref).addRefTarget(cp.ref, secref)
    } else {
      ctx.addRefTarget(ref1, SastRef(cwf, sec))
    }
  }

  def convertBlock(tlblock: SBlock)(ctx: Cta): CtxCS = {
    // make all blocks labellable
    val refctx = tlblock.attributes.named.get("label") match {
      case None      => ctx
      case Some(ref) => ctx.addRefTarget(ref, SastRef(cwf, tlblock))
    }
    tlblock.content match {
      case Paragraph(content) =>
        convertInlines(content.inline)(refctx)
          .map(il => SBlock(tlblock.attributes, Paragraph(Text(il.toList))): Sast).single

      case Parsed(delimiter, blockContent) =>
        convertSeq(blockContent)(refctx).map(bc =>
          SBlock(tlblock.attributes, Parsed(delimiter, bc.toList)): Sast
        ).single

      case Fenced(text) =>
        if (tlblock.attributes.named.contains("converter")) {
          val resctx = converter.convert(tlblock).schedule(refctx)
          convertSingle(resctx.data)(resctx)
        } else {
          // fenced blocks allow line labels
          tlblock.attributes.named.get("label") match {
            case None => ctx.retc(tlblock)
            case Some(ref) =>
              val matches = """:§([^§]*?)§""".r.findAllMatchIn(text).map(_.group(1)).toList
              val target  = SastRef(cwf, tlblock)
              matches.foldLeft(ctx.ret(target)) { (cx, group) => cx.addRefTarget(ref + group, target) }.retc(tlblock)
          }

        }

      case SpaceComment(content) => ctx.retc(tlblock)

    }
  }

  def convertInlines(inners: Seq[Inline])(implicit ctx: Cta): Ctx[Chain[Inline]] =
    ctx.fold(inners) { (ctx, inline) =>
      inline match {
        case inlineText: InlineText => ctx.retc(inlineText)
        case m: Macro               => convertMacro(m)(ctx).map(identity[Inline]).single
      }
    }

  def convertMacro(mcro: Macro)(implicit ctx: Cta): Ctx[Macro] =
    mcro match {
      case Macro(Image, attributes) if attributes.named.contains("converter") =>
        val resctx = converter.convert(cwd, mcro).schedule(ctx)
        convertMacro(resctx.data)(resctx)

      case Macro(Image, attributes) if converter.requiresConversion(attributes.target) =>
        project.resolve(cwd, attributes.target).fold(ctx.ret(mcro)) { file =>
          val resctx    = converter.applyConversion(file).schedule(ctx)
          val reltarget = cwd.relativize(resctx.data)
          convertMacro(Macro(
            Image,
            attributes.copy(
              raw = attributes.raw.init :+ Attribute("", reltarget.toString)
            )
          ))(resctx)
        }

      case Macro(Label, attributes) =>
        ctx.addRefTarget(attributes.target, SastRef(cwf, ctx.stack.head)).ret(mcro)

      case Macro(Include, attributes) if attributes.arguments.isEmpty =>
        project.resolve(cwd, attributes.target) match {
          case None =>
            scribe.error(s"unknown include ${attributes.target}" + reporter(mcro))
            ctx.ret(mcro)
          case Some(file) => ctx.copy(includes = file :: ctx.includes).ret(mcro)
        }

      case other => ctx.ret(other)

    }

}
