package scitzen.outputs

import de.rmgk.Chain
import scitzen.contexts.SastContext
import scitzen.generic.{ArticleRef, SastRef}
import scitzen.sast.*
import scitzen.sast.DCommand.{BibQuery, Cite, Image, Index, Ref}

class SastToSastConverter(articleRef: ArticleRef):

  def document = articleRef.document

  type CtxCS   = SastContext[Chain[Sast]]
  type Ctx[+T] = SastContext[T]
  type Cta     = Ctx[?]

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
    val aliases  = ref1 :: newLabel :: attr.plain("aliases").toList.flatMap(_.split(',').toList)
    counter.ret((aliases, attr.updated("unique ref", newLabel)))

  def convertSeq(b: Seq[Sast])(ctx: Cta): CtxCS =
    ctx.fold(b) { (ctx, sast) => convertSingle(sast)(ctx).single }

  def convertSingle(sast: Sast)(ctx: Cta): Ctx[Sast] =
    sast match
      case tlBlock: Block =>
        convertBlock(tlBlock)(ctx)

      case sec @ Section(title, level, _) =>
        val ctxWithRef = ensureSectionRef(sec, ctx)
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
          Slist(cs.toSeq)
        }

      case mcro: Directive =>
        convertMacro(mcro)(ctx)

  def convertBlock(block: Block)(ctx: Cta): Ctx[Sast] =
    // make all blocks labellable
    val refctx: Ctx[Block] = ensureBlockRef(block, ctx)
    val ublock             = refctx.data
    val resctx = ublock.content match
      case Paragraph(content) =>
        convertInlines(content.inl)(refctx)
          .map(il => ublock.copy(content = Paragraph(Text(il.toList)))(ublock.prov))

      case Parsed(delimiter, blockContent) =>
        convertSeq(blockContent)(refctx).map(bc =>
          ublock.copy(content = Parsed(delimiter, bc.toList))(ublock.prov)
        )

      case SpaceComment(_) | Fenced(_) => refctx.ret(ublock)
    if resctx.data.command == BCommand.Convert
    then resctx.addConversionBlock(resctx.data)
    else resctx

  private def ensureSectionRef(sec: Section, ctx: Cta) = {
    val resctx          = ensureUniqueRef(ctx, sec.autolabel, sec.attributes)
    val (aliases, attr) = resctx.data
    val ublock          = sec.copy(attributes = attr)(sec.prov)
    val target          = SastRef(ublock, articleRef)
    refAliases(resctx, aliases, target).ret(ublock)
  }

  private def ensureBlockRef(block: Block, ctx: Cta) = {
    block.attributes.plain("label") match
      case None => ctx.ret(block)
      case Some(ref) =>
        val resctx          = ensureUniqueRef(ctx, ref, block.attributes)
        val (aliases, attr) = resctx.data
        val ublock          = block.copy(attributes = attr)(block.prov)
        val target          = SastRef(ublock, articleRef)
        refAliases(resctx, aliases, target).ret(ublock)
  }

  private def refAliases(resctx: Ctx[?], aliases: List[String], target: SastRef): Ctx[Unit] =
    aliases.foldLeft(resctx.ret(()))((c: Ctx[?], a) => c.addRefTarget(a, target).ret(()))

  def convertText(text: Text, ctx: Cta) = convertInlines(text.inl)(ctx).map(il => Text(il.toList))

  def convertInlines(inners: Seq[Inline])(ctx: Cta): Ctx[Chain[Inline]] =
    ctx.fold(inners) { (ctx, inline) =>
      inline match
        case inlineText: InlineText => ctx.ret(Chain.one(inlineText))
        case m: Directive           => convertMacro(m)(ctx).single
    }

  def convertMacro(directive: Directive)(ctx: Cta): Ctx[Directive] =
    directive.command match
      case Image           => ctx.addImage(directive).ret(directive)
      case Ref | Index     => ctx.addReference(directive).ret(directive)
      case Cite | BibQuery =>
        // TODO this is a temporary way to rename the parameter, remove at some point
        val res =
          val style = directive.attributes.plain("style")
          if style.contains("name") then
            directive.copy(attributes = directive.attributes.updated("style", "author"))(directive.prov)
          else
            directive
        ctx.addCitation(res).ret(res)
      case _ => ctx.ret(directive)
