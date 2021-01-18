package scitzen.outputs

import better.files.File
import cats.data.Chain
import scitzen.extern.ImageConverter
import scitzen.generic.{Article, ConversionContext, Document, DocumentDirectory, Project, Reporter, SastRef}
import scitzen.sast.MacroCommand.{Image, Include}
import scitzen.sast.{
  Attribute, Attributes, Block, Fenced, Inline, InlineText, ListItem, Macro, Paragraph, Parsed, Sast, Section, Slist,
  SpaceComment, Text
}

class SastToSastConverter(
    project: Project,
    cwf: File,
    reporter: Reporter,
    converter: Option[ImageConverter]
) {

  type CtxCS  = ConversionContext[Chain[Sast]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[_]

  val uid = Integer.toHexString(cwf.hashCode())

  def artOpt(ctx: Cta, self: Option[Section] = None): Option[Article] = {
    (self ++: ctx.sections).find(!Article.notArticleHeader(_)).collect {
      case sect @ Section(_, "=", _) => Article(sect, Nil, Document(cwf, "", Nil, Nil), DocumentDirectory(Nil))
    }
  }

  val cwd = if (cwf.isDirectory) cwf else cwf.parent

  def convertSeq(b: Seq[Sast])(implicit ctx: Cta): CtxCS = {
    ctx.fold(b) { (ctx, sast) => convertSingle(sast)(ctx).single }
  }

  def convertSingle(sast: Sast)(implicit ctx: Cta): Ctx[Sast] =
    sast match {
      case tlBlock: Block => convertBlock(tlBlock)(ctx)

      case sec @ Section(title, level, _) =>
        val (newSection, ctxWithRef) =
          addRefTargetMakeUnique(ctx, sec, sec.ref, sec.attributes)(a => sec.copy(attributes = a))
        val conCtx = ctxWithRef.push(newSection)
        convertInlines(title.inl)(conCtx).map { title =>
          Section(Text(title.toList), level, newSection.attributes)
        }

      case Slist(children) =>
        ctx.fold[ListItem, ListItem](children) { (ctx, child) =>
          child.content match {
            case None => ctx.retc(child)
            case Some(content) =>
              convertSingle(content)(ctx).map { con =>
                Chain(ListItem(child.marker, child.text, Some(con)))
              }
          }
        }.map { cs =>
          scitzen.sast.Slist(cs.iterator.toSeq)
        }

      case mcro @ Macro(_, _) =>
        convertMacro(mcro).map(identity(_): Sast)
    }

  def addRefTargetMakeUnique[A <: Sast](
      ctx: Cta,
      sec: A,
      ref1: String,
      attr: Attributes,
  )(updateAttr: (Attributes) => A): (A, Ctx[Unit]) = {
    val counter =
      if (ctx.labelledThings.contains(ref1)) {
        ctx.nextId.map(_.toString)
      } else {
        ctx.ret("")
      }
    val newLabel = s"$ref1 ($uid${counter.data})"
    val cp       = updateAttr(attr.updated("label", newLabel))

    val asSection = Option.when(cp.isInstanceOf[Section])(cp.asInstanceOf[Section])
    val secref    = SastRef(cwf, cp, artOpt(ctx, asSection))
    val aliases   = attr.named.get("aliases").toList.flatMap(_.split(',').toList)
    val referenced = (ref1 :: newLabel :: aliases).foldLeft(counter.ret(())) { (c, l) =>
      c.addRefTarget(l, secref).ret(())
    }
    (cp, referenced)
  }

  def convertBlock(block: Block)(ctx: Cta): Ctx[Sast] = {
    // make all blocks labellable
    val (ublock, refctx) = block.attributes.named.get("label") match {
      case None => (block, ctx)
      case Some(ref) =>
        addRefTargetMakeUnique(ctx, block, ref, block.attributes)(a => block.copy(attributes = a))
    }
    ublock.content match {
      case Paragraph(content) =>
        convertInlines(content.inl)(refctx)
          .map(il => Block(ublock.attributes, Paragraph(Text(il.toList))): Sast)

      case Parsed(delimiter, blockContent) =>
        convertSeq(blockContent)(refctx).map(bc => Block(ublock.attributes, Parsed(delimiter, bc.toList)): Sast)

      case Fenced(text) =>
        if (ublock.attributes.named.contains("converter") && converter.isDefined) {
          val resctx = converter.get.convertBlock(cwd, ublock).schedule(refctx)
          convertSingle(resctx.data)(resctx)
        } else {
          refctx.ret(ublock)
        }

      case SpaceComment(_) => refctx.ret(ublock)

    }
  }

  def convertInlines(inners: Seq[Inline])(implicit ctx: Cta): Ctx[Chain[Inline]] =
    ctx.fold(inners) { (ctx, inline) =>
      inline match {
        case inlineText: InlineText => ctx.retc(inlineText)
        case m: Macro               => convertMacro(m)(ctx).single
      }
    }

  def convertMacro(mcro: Macro)(implicit ctx: Cta): Ctx[Macro] =
    mcro match {

      // explicit image conversions
      case Macro(Image, attributes) if attributes.named.contains("converter") && converter.isDefined =>
        val resctx = converter.get.convertMacroTargetFile(cwd, mcro).schedule(ctx)
        convertMacro(resctx.data)(resctx)

      // unsupported image format conversions
      case Macro(Image, attributes) if converter.isDefined && converter.get.requiresConversion(attributes.target) =>
        project.resolve(cwd, attributes.target).fold(ctx.ret(mcro)) { file =>
          val resctx    = converter.get.applyConversion(file).schedule(ctx)
          val reltarget = cwd.relativize(resctx.data)
          convertMacro(Macro(
            Image,
            attributes.copy(
              raw = attributes.raw.init :+ Attribute("", reltarget.toString)
            )
          ))(resctx)
        }

      // collect image macros
      case mcro @ Macro(Image, _) => ctx.addMacro(mcro).ret(mcro)

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
