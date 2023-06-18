package scitzen.outputs

import de.rmgk.Chain
import scitzen.cli.ConversionAnalysis
import scitzen.compat.Logging.scribe
import scitzen.contexts.ConversionContext
import scitzen.generic.Article
import scitzen.sast.*

abstract class ProtoConverter[ResultT](
    article: Article,
    anal: ConversionAnalysis
):

  val project  = article.doc.path.project
  val reporter = article.doc.reporter

  type CtxCF  = ConversionContext[Chain[ResultT]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[?]

  val videoEndings = List(".mp4", ".mkv", ".webm")

  def run() = convertSastSeq(article.sast)

  def convertSastSeq(b: Iterable[Sast], ctx: Cta): CtxCF = ctx.fold(b)((ctx, sast) => convertSast(sast, ctx))

  def convertSast(singleSast: Sast, ctx: Cta): CtxCF =
    singleSast match
      case block: Block         => convertBlock(block, ctx)
      case directive: Directive => convertDirective(directive, ctx)
      case section: Section     => convertSection(section, ctx)
      case slist: Slist         => convertSlist(slist, ctx)

  def convertBlock(sBlock: Block, ctx: Cta): CtxCF
  def convertDirective(directive: Directive, ctx: Cta): CtxCF = convertInlineDirective(directive, ctx)
  def convertSection(section: Section, ctx: Cta): CtxCF
  def convertSlist(slist: Slist, ctx: Cta): CtxCF

  def convertInlineSeq(inlines: Iterable[Inline], ctx: Cta): CtxCF =
    ctx.fold(inlines) { (ctx, inline) => convertInline(inline, ctx) }

  def convertInline(inlineSast: Inline, ctx: Cta): CtxCF =
    inlineSast match
      case inlineText: InlineText => convertInlineText(inlineText, ctx)
      case directive: Directive   => convertDirective(directive, ctx)

  def convertInlineText(inlineText: InlineText, ctx: Cta): CtxCF
  def convertInlineDirective(directive: Directive, ctx: Cta): CtxCF

  def warn(msg: String, im: Directive): String =
    val macroStr = SastToScimConverter(anal.bib).macroToScim(im)
    scribe.warn(s"$msg: ⸢$macroStr⸥${article.doc.reporter(im)}")
    macroStr
