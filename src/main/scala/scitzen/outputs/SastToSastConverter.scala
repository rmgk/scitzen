package scitzen.outputs

import better.files.File
import cats.data.Chain
import scitzen.contexts.SastContext
import scitzen.extern.ImageConverter
import scitzen.generic.{Article, Document, Project, SastRef}
import scitzen.sast.MacroCommand.{Image, Include}
import scitzen.sast._

class SastToSastConverter(
    project: Project,
    document: Document,
    converter: Option[ImageConverter]
) {

  type CtxCS  = SastContext[Chain[Sast]]
  type Ctx[T] = SastContext[T]
  type Cta    = Ctx[_]

  def cwf: File   = document.file
  val cwd: File   = if (cwf.isDirectory) cwf else cwf.parent
  val uid: String = Integer.toHexString(cwf.hashCode())

  def run(): CtxCS = convertSeq(document.sast)(SastContext(()))

  def findArticle(ctx: Cta, self: Section): Option[Article] = {
    (self +: ctx.sections).find(!Article.notArticleHeader(_)).collect {
      case sect @ Section(_, "=", _) => Article(sect, Nil, Document(cwf, "", Nil))
    }
  }

  def ensureUniqueRef[A <: Sast](
      ctx: Cta,
      ref1: String,
      attr: Attributes,
  ): Ctx[(List[String], Attributes)] = {
    val counter =
      if (ctx.labelledThings.contains(ref1)) {
        ctx.nextId.map(_.toString)
      } else {
        ctx.ret("")
      }
    val newLabel = s"$ref1 ($uid${counter.data})"
    val aliases  = ref1 :: newLabel :: attr.named.get("aliases").toList.flatMap(_.split(',').toList)
    counter.ret((aliases, attr.updated("label", newLabel)))
  }

  def convertSeq(b: Seq[Sast])(implicit ctx: Cta): CtxCS = {
    ctx.fold(b) { (ctx, sast) => convertSingle(sast)(ctx).single }
  }

  def convertSingle(sast: Sast)(implicit ctx: Cta): Ctx[Sast] =
    sast match {
      case tlBlock: Block => convertBlock(tlBlock)(ctx)

      case sec @ Section(title, level, _) =>
        val ctxWithRef = {
          val resctx          = ensureUniqueRef(ctx, sec.ref, sec.attributes)
          val (aliases, attr) = resctx.data
          val ublock          = sec.copy(attributes = attr)
          val target          = SastRef(cwf, ublock, findArticle(ctx, sec))
          refAliases(resctx, aliases, target).ret(ublock)
        }
        val newSection = ctxWithRef.data
        val conCtx     = ctxWithRef.push(newSection)
        convertInlines(title.inl)(conCtx).map { title =>
          Section(Text(title.toList), level, newSection.attributes)
        }

      case Slist(children) =>
        ctx.fold[ListItem, ListItem](children) { (ctx, child) =>
          child.content match {
            case None => ctx.ret(Chain(child))
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

  def convertBlock(block: Block)(ctx: Cta): Ctx[Sast] = {
    // make all blocks labellable
    val refctx: Ctx[Block] = block.attributes.named.get("label") match {
      case None => ctx.ret(block)
      case Some(ref) =>
        val resctx          = ensureUniqueRef(ctx, ref, block.attributes)
        val (aliases, attr) = resctx.data
        val ublock          = block.copy(attributes = attr)
        val target          = SastRef(cwf, ublock, None)
        refAliases(resctx, aliases, target).ret(ublock)
    }
    val ublock = refctx.data
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

  private def refAliases(resctx: Ctx[_], aliases: List[String], target: SastRef): Ctx[Unit] = {
    aliases.foldLeft(resctx.ret(()))((c: Ctx[_], a) => c.addRefTarget(a, target).ret(()))
  }

  def convertInlines(inners: Seq[Inline])(implicit ctx: Cta): Ctx[Chain[Inline]] =
    ctx.fold(inners) { (ctx, inline) =>
      inline match {
        case inlineText: InlineText => ctx.ret(Chain.one(inlineText))
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
            scribe.error(s"unknown include ${attributes.target}" + document.reporter(mcro))
            ctx.ret(mcro)
          case Some(file) => ctx.copy(includes = file :: ctx.includes).ret(mcro)
        }

      case other => ctx.ret(other)

    }

}
