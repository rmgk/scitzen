package scitzen.outputs

import better.files.File
import cats.data.Chain
import scitzen.contexts.SastContext
import scitzen.extern.{Hashes, ITargetPrediction}
import scitzen.generic.{Article, Document, Project, SastRef}
import scitzen.sast.MacroCommand.{Image, Include}
import scitzen.sast._

class SastToSastConverter(document: Document, project: Project):

  type CtxCS  = SastContext[Chain[Sast]]
  type Ctx[T] = SastContext[T]
  type Cta    = Ctx[?]

  def cwf: File        = document.file
  def cwd: File        = document.file.parent
  val uid: String      = Integer.toHexString(cwf.hashCode())
  val targetPrediction = ITargetPrediction(project, cwf.parent)

  def run(): CtxCS = convertSeq(document.sast)(SastContext(()))

  def findArticle(ctx: Cta, self: Section): Option[Article] =
    (self +: ctx.sections).find(!Article.notArticleHeader(_)).collect {
      case sect @ Section(_, "=", _, _) => Article(sect, Nil, Document(cwf, "", Nil))
    }

  def ensureUniqueRef[A <: Sast](
      ctx: Cta,
      ref1: String,
      attr: Attributes,
  ): Ctx[(List[String], Attributes)] =
    val counter =
      if ctx.labelledThings.contains(ref1) then
        ctx.nextId.map(_.toString)
      else
        ctx.ret("")
    val newLabel = s"$ref1 ($uid${counter.data})"
    val aliases  = ref1 :: newLabel :: attr.named.get("aliases").toList.flatMap(_.split(',').toList)
    counter.ret((aliases, attr.updated("label", newLabel)))

  def convertSeq(b: Seq[Sast])(ctx: Cta): CtxCS =
    ctx.fold(b) { (ctx, sast) => convertSingle(sast)(ctx).single }

  def convertSingle(sast: Sast)(ctx: Cta): Ctx[Sast] =
    sast match
      case tlBlock: Block => convertBlock(tlBlock)(ctx)

      case sec @ Section(title, level, _, _) =>
        val ctxWithRef =
          val resctx          = ensureUniqueRef(ctx, sec.ref, sec.attributes)
          val (aliases, attr) = resctx.data
          val ublock          = sec.copy(attributes = attr)
          val target          = SastRef(cwf, ublock, findArticle(ctx, sec))
          refAliases(resctx, aliases, target).ret(ublock)
        val newSection = ctxWithRef.data
        val conCtx     = ctxWithRef.addSection(newSection)
        convertInlines(title.inl)(conCtx).map { title =>
          Section(Text(title.toList), level, newSection.attributes, newSection.prov)
        }

      case Slist(children) =>
        ctx.fold[ListItem, ListItem](children) { (ctx, child) =>
          child.content match
            case None => ctx.ret(Chain(child))
            case Some(content) =>
              convertSingle(content)(ctx).map { con =>
                Chain(ListItem(child.marker, child.text, Some(con)))
              }
        }.map { cs =>
          scitzen.sast.Slist(cs.iterator.toSeq)
        }

      case mcro: Macro =>
        convertMacro(mcro)(ctx).map(identity(_): Sast)

  def convertBlock(block: Block)(ctx: Cta): Ctx[Sast] =
    // make all blocks labellable
    val refctx: Ctx[Block] = block.attributes.named.get("label") match
      case None => ctx.ret(block)
      case Some(ref) =>
        val resctx          = ensureUniqueRef(ctx, ref, block.attributes)
        val (aliases, attr) = resctx.data
        val ublock          = block.copy(attributes = attr)
        val target          = SastRef(cwf, ublock, None)
        refAliases(resctx, aliases, target).ret(ublock)
    val refblock      = refctx.data
    val refattributes = refblock.attributes
    val ublock        = makeAbsolute(refattributes, "template").fold(refblock)(a => refblock.copy(attributes = a))
    ublock.content match
      case Paragraph(content) =>
        convertInlines(content.inl)(refctx)
          .map(il => Block(ublock.attributes, Paragraph(Text(il.toList)), ublock.prov): Sast)

      case Parsed(delimiter, blockContent) =>
        convertSeq(blockContent)(refctx).map(bc =>
          Block(ublock.attributes, Parsed(delimiter, bc.toList), ublock.prov): Sast
        )

      case Fenced(text) =>
        if ublock.attributes.named.contains("converter") then
          val contentHash = Hashes.sha1hex(text)
          val hashedBlock = ublock.copy(attributes = ublock.attributes.updated("content hash", contentHash))
          val newBlock    = hashedBlock.copy(attributes = targetPrediction.predictBlock(hashedBlock.attributes))
          refctx.addConversionBlock(newBlock).ret(newBlock)
        else refctx.ret(ublock)

      case SpaceComment(_) => refctx.ret(ublock)

  private def makeAbsolute(attributes: Attributes, select: String = ""): Option[Attributes] =
    val attr =
      if select == "" then
        attributes.positional.lastOption
      else attributes.named.get(select)
    attr.map { template =>
      val abs = project.resolve(cwd, template)
      val rel = project.relativizeToProject(abs.get)
      if select == "" then
        Attributes(attributes.raw.map { attr =>
          if attr.id == "" && attr.value == attributes.target then Attribute("", rel.toString)
          else attr
        })
      else attributes.updated(select, rel.toString)
    }
  private def refAliases(resctx: Ctx[?], aliases: List[String], target: SastRef): Ctx[Unit] =
    aliases.foldLeft(resctx.ret(()))((c: Ctx[?], a) => c.addRefTarget(a, target).ret(()))

  def convertInlines(inners: Seq[Inline])(ctx: Cta): Ctx[Chain[Inline]] =
    ctx.fold(inners) { (ctx, inline) =>
      inline match
        case inlineText: InlineText => ctx.ret(Chain.one(inlineText))
        case m: Macro               => convertMacro(m)(ctx).single
    }

  def convertMacro(initial: Macro)(ctx: Cta): Ctx[Macro] =
    val mcro =
      initial.command match
        case Image | Include => makeAbsolute(initial.attributes).fold(initial)(a => initial.copy(attributes = a))
        case _               => initial
    mcro.command match
      case Image =>
        val enhanced = mcro.copy(attributes = targetPrediction.predictMacro(mcro.attributes))
        ctx.addImage(enhanced).ret(enhanced)
      case _ => ctx.ret(mcro)
