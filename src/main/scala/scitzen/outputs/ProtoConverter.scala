package scitzen.outputs

import de.rmgk.Chain
import scitzen.cli.ConversionAnalysis
import scitzen.compat.Logging.scribe
import scitzen.contexts.ConversionContext
import scitzen.generic.Article
import scitzen.sast.*

abstract class ProtoConverter[BlockRes, InlineRes](
    article: Article,
    anal: ConversionAnalysis
):

  val project  = article.doc.path.project
  val reporter = article.doc.reporter

  type CtxCF  = ConversionContext[Chain[BlockRes]]
  type CtxInl = ConversionContext[Chain[InlineRes]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[?]

  val videoEndings = List(".mp4", ".mkv", ".webm")

  def convertSastSeq(b: Iterable[Sast], ctx: Cta): CtxCF = ctx.fold(b)((ctx, sast) => convertSast(sast, ctx))

  def convertSast(singleSast: Sast, ctx: Cta): CtxCF =
    singleSast match
      case block: Block =>
        if block.command == BCommand.Convert then
          convertSastSeq(anal.block.substitute(block), ctx)
        else convertBlock(block, ctx)
      case directive: Directive => convertDirective(directive, ctx)
      case section: Section     => convertSection(section, ctx)
      case slist: Slist         => convertSlist(slist, ctx)

  def convertBlock(sBlock: Block, ctx: Cta): CtxCF
  def convertDirective(directive: Directive, ctx: Cta): CtxCF =
    convertInlineDirective(directive, ctx).map(v => Chain(inlineResToBlock(v)))
  def convertSection(section: Section, ctx: Cta): CtxCF
  def convertSlist(slist: Slist, ctx: Cta): CtxCF

  def convertInlinesAsBlock(inlines: Iterable[Inline], ctx: Cta): Ctx[BlockRes] =
    convertInlineSeq(inlines, ctx).map(v => inlineResToBlock(v))
  def inlineResToBlock(inl: Chain[InlineRes]): BlockRes

  def convertInlineSeq(inlines: Iterable[Inline], ctx: Cta): CtxInl =
    ctx.fold(inlines) { (ctx, inline) => convertInline(inline, ctx) }

  def convertInline(inlineSast: Inline, ctx: Cta): CtxInl =
    inlineSast match
      case inlineText: InlineText => convertInlineText(inlineText, ctx)
      case directive: Directive   => convertInlineDirective(directive, ctx)

  def convertInlineText(inlineText: InlineText, ctx: Cta): CtxInl
  def convertInlineDirective(directive: Directive, ctx: Cta): CtxInl

  def warn(msg: String, im: Directive): String =
    val macroStr = SastToScimConverter(anal.bib).macroToScim(im)
    scribe.warn(s"$msg: ⸢$macroStr⸥${article.doc.reporter(im)}")
    macroStr
