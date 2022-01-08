package scitzen.outputs

import better.files.File
import cats.data.Chain
import scitzen.contexts.SastContext
import scitzen.extern.{Hashes, ITargetPrediction}
import scitzen.generic.{Article, Document, Project, SastRef}
import scitzen.sast.*
import scitzen.sast.Attribute.Positional
import scitzen.sast.DCommand.{Image, Include}

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
      case sect @ Section(_, "=", _) => Article(sect, Nil, Document(cwf, "", Nil))
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

      case sec @ Section(title, level, _) =>
        val ctxWithRef =
          val resctx          = ensureUniqueRef(ctx, sec.ref, sec.attributes)
          val (aliases, attr) = resctx.data
          val ublock          = sec.copy(attributes = attr)(sec.prov)
          val target          = SastRef(cwf, ublock, findArticle(ctx, sec))
          refAliases(resctx, aliases, target).ret(ublock)
        val newSection = ctxWithRef.data
        val conCtx     = ctxWithRef.addSection(newSection)
        convertInlines(title.inl)(conCtx).map { title =>
          Section(Text(title.toList), level, newSection.attributes)(newSection.prov)
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

      case mcro: Directive =>
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

        val runctx = if ublock.command == "execute" && ublock.attributes.named.get("lang").contains("js") then
          val res = scitzen.extern.JsRunner().run(text, ublock.attributes)
          refctx.ret(ublock.copy(content = Fenced(res.toString)))
        else refctx

        val runblock = runctx.data

        if runblock.attributes.named.contains("converter") then
          val contentHash = Hashes.sha1hex(text)
          val hashedBlock = runblock.copy(attributes = runblock.attributes.updated("content hash", contentHash))
          val newBlock    = hashedBlock.copy(attributes = targetPrediction.predictBlock(hashedBlock.attributes))
          runctx.addConversionBlock(newBlock).ret(newBlock)
        else runctx.ret(runblock)

      case SpaceComment(_) => refctx.ret(ublock)

  private def makeAbsolute(attributes: Attributes, select: String = ""): Option[Attributes] =
    val attr =
      if select == "" then
        attributes.legacyPositional.lastOption
      else attributes.named.get(select)
    attr.flatMap { template =>
      val abs = project.resolve(cwd, template)
      abs.map(project.relativizeToProject).map { rel =>
        if select == "" then
          Attributes(attributes.raw.map {
            case Positional(text, Some(value)) if value == attributes.target => Attribute("", rel.toString)
            case other                                             => other
          })
        else attributes.updated(select, rel.toString)
      }
    }
  private def refAliases(resctx: Ctx[?], aliases: List[String], target: SastRef): Ctx[Unit] =
    aliases.foldLeft(resctx.ret(()))((c: Ctx[?], a) => c.addRefTarget(a, target).ret(()))

  def convertInlines(inners: Seq[Inline])(ctx: Cta): Ctx[Chain[Inline]] =
    ctx.fold(inners) { (ctx, inline) =>
      inline match
        case inlineText: InlineText => ctx.ret(Chain.one(inlineText))
        case m: Directive           => convertMacro(m)(ctx).single
    }

  def convertMacro(initial: Directive)(ctx: Cta): Ctx[Directive] =
    val mcro =
      initial.command match
        case Image | Include => makeAbsolute(initial.attributes).fold(initial)(a => initial.copy(attributes = a)(initial.prov))
        case _               => initial
    mcro.command match
      case Image =>
        val enhanced = mcro.copy(attributes = targetPrediction.predictMacro(mcro.attributes))(mcro.prov)
        ctx.addImage(enhanced).ret(enhanced)
      case _ => ctx.ret(mcro)
