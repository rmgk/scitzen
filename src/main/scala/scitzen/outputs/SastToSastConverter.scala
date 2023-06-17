package scitzen.outputs

import de.rmgk.Chain
import scitzen.contexts.SastContext
import scitzen.extern.{Hashes, ITargetPrediction}
import scitzen.generic.{Document, Project, SastRef}
import scitzen.sast.*
import scitzen.sast.DCommand.{BibQuery, Cite, Image}

import java.nio.file.Path

class SastToSastConverter(document: Document, fullSast: List[Sast], project: Project):

  type CtxCS  = SastContext[Chain[Sast]]
  type Ctx[T] = SastContext[T]
  type Cta    = Ctx[?]

  def cwf: Path        = document.path.absolute
  val targetPrediction = ITargetPrediction(project, cwf.getParent)

  def run(): CtxCS = convertSeq(fullSast)(SastContext(()))

  def findArticle(ctx: Cta, self: Section): Option[Section] =
    fullSast.headOption match
      case Some(sect @ Section(_, "=", _)) => Some(sect)
      case other                           => None

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
    val newLabel = s"$ref1 (${document.uid}${counter.data})"
    val aliases  = ref1 :: newLabel :: attr.named.get("aliases").toList.flatMap(_.split(',').toList)
    counter.ret((aliases, attr.updated("unique ref", newLabel)))

  def convertSeq(b: Seq[Sast])(ctx: Cta): CtxCS =
    ctx.fold(b) { (ctx, sast) => convertSingle(sast)(ctx).single }

  def convertSingle(sast: Sast)(ctx: Cta): Ctx[Sast] =
    sast match
      case tlBlock: Block => convertBlock(tlBlock)(ctx)

      case sec @ Section(title, level, _) =>
        val ctxWithRef =
          val resctx          = ensureUniqueRef(ctx, sec.autolabel, sec.attributes)
          val (aliases, attr) = resctx.data
          val ublock          = sec.copy(attributes = attr)(sec.prov)
          val target          = SastRef(document.path, ublock, findArticle(ctx, sec))
          refAliases(resctx, aliases, target).ret(ublock)
        val newSection = ctxWithRef.data
        val conCtx     = ctxWithRef.addSection(newSection)
        convertInlines(title.inl)(conCtx).map { title =>
          Section(Text(title.toList), level, newSection.attributes)(newSection.prov)
        }

      case Slist(children) =>
        ctx.fold[ListItem, ListItem](children) { (origctx, origchild) =>
          val textctx = convertText(origchild.text, origctx)
          val contentctx = origchild.content match
            case None => textctx.map(_ => None)
            case Some(content) =>
              convertSingle(content)(textctx).map(c => Some(c))

          contentctx.ret {
            Chain(ListItem(origchild.marker, textctx.data, contentctx.data))
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
        val target          = SastRef(document.path, ublock, None)
        refAliases(resctx, aliases, target).ret(ublock)
    val ublock = refctx.data
    ublock.content match
      case Paragraph(content) =>
        convertInlines(content.inl)(refctx)
          .map(il => Block(ublock.attributes, Paragraph(Text(il.toList)), ublock.prov): Sast)

      case Parsed(delimiter, blockContent) =>
        convertSeq(blockContent)(refctx).map(bc =>
          Block(ublock.attributes, Parsed(delimiter, bc.toList), ublock.prov): Sast
        )

      case Fenced(text) =>
        val runctx =
          if ublock.command == "execute" && ublock.attributes.named.get("lang").contains("js") then
            val res = scitzen.extern.JsRunner().run(text, ublock.attributes)
            refctx.ret(ublock.copy(attributes = ublock.attributes.updated("exec result", res)))
          else refctx

        val runblock = runctx.data

        if runblock.attributes.named.contains("converter") then
          val contentHash = Hashes.sha1hex(text)
          val hashedBlock = runblock.copy(attributes = runblock.attributes.updated("content hash", contentHash))
          val newBlock    = hashedBlock.copy(attributes = targetPrediction.predictBlock(hashedBlock.attributes))
          runctx.addConversionBlock(newBlock).ret(newBlock)
        else runctx.ret(runblock)

      case SpaceComment(_) => refctx.ret(ublock)

  private def refAliases(resctx: Ctx[?], aliases: List[String], target: SastRef): Ctx[Unit] =
    aliases.foldLeft(resctx.ret(()))((c: Ctx[?], a) => c.addRefTarget(a, target).ret(()))

  def convertText(text: Text, ctx: Cta) = convertInlines(text.inl)(ctx).map(il => Text(il.toList))

  def convertInlines(inners: Seq[Inline])(ctx: Cta): Ctx[Chain[Inline]] =
    ctx.fold(inners) { (ctx, inline) =>
      inline match
        case inlineText: InlineText => ctx.ret(Chain.one(inlineText))
        case m: Directive           => convertMacro(m)(ctx).single
    }

  def convertMacro(mcro: Directive)(ctx: Cta): Ctx[Directive] =
    mcro.command match
      case Image =>
        val enhanced = mcro.copy(attributes = targetPrediction.predictMacro(mcro.attributes))(mcro.prov)
        ctx.addImage(enhanced).ret(enhanced)
      case Cite | BibQuery =>
        val res =
          val style = mcro.attributes.named.get("style")
          if style.contains("name") then
            mcro.copy(attributes = mcro.attributes.prepend(List(Attribute("style", "author"))))(mcro.prov)
          else
            mcro
        ctx.addCitation(res).ret(res)
      case _ => ctx.ret(mcro)
