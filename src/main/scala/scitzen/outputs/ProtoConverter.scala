package scitzen.outputs

import de.rmgk.Chain
import scitzen.cli.ConversionAnalysis
import scitzen.compat.Logging.scribe
import scitzen.contexts.ConversionContext
import scitzen.extern.ImageTarget
import scitzen.generic.{Article, ProjectPath}
import scitzen.sast.*

import java.nio.file.Files

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

  def subconverter(article: Article, analysis: ConversionAnalysis): ProtoConverter[BlockRes, InlineRes]

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
    convertInlineDirective(directive, ctx).map(v => Chain(inlinesAsToplevel(v)))
  def convertSection(section: Section, ctx: Cta): CtxCF
  def convertSlist(slist: Slist, ctx: Cta): CtxCF

  def convertInlinesAsBlock(inlines: Iterable[Inline], ctx: Cta): Ctx[BlockRes] =
    convertInlineSeq(inlines, ctx).map(v => inlineResToBlock(v))

  def inlineResToBlock(inl: Chain[InlineRes]): BlockRes
  def inlinesAsToplevel(inl: Chain[InlineRes]): BlockRes

  def convertInlineSeq(inlines: Iterable[Inline], ctx: Cta): CtxInl =
    ctx.fold(inlines) { (ctx, inline) => convertInline(inline, ctx) }

  def convertInline(inlineSast: Inline, ctx: Cta): CtxInl =
    inlineSast match
      case inlineText: InlineText => convertInlineText(inlineText, ctx)
      case directive: Directive   => convertInlineDirective(directive, ctx)

  def convertInlineText(inlineText: InlineText, ctx: Cta): CtxInl
  def convertInlineDirective(directive: Directive, ctx: Cta): CtxInl

  def convertImage(ctx: Cta, directive: Directive, imageTarget: ImageTarget)(cont: ProjectPath => CtxInl): CtxInl =
    val target = anal.image.lookup(article.doc.resolve(directive.attributes.target).get, imageTarget)
    if !Files.exists(target.absolute)
    then ctx.retc(warn(s"could not find path", directive))
    else cont(target)

  def stringToInlineRes(str: String): InlineRes

  def warn(msg: String, im: Directive): InlineRes =
    val macroStr = SastToScimConverter(anal.bib).macroToScim(im)
    scribe.warn(s"$msg: ⸢$macroStr⸥${article.doc.reporter(im)}")
    stringToInlineRes(macroStr)

  def addDetail(ctx: CtxCF): CtxCF

  def handleAggregate(ctx: Cta, directive: Directive): ConversionContext[Chain[BlockRes]] = {
    val pathpart = article.doc.path.directory.resolve(directive.attributes.target).normalize()
    val found = anal.directory.byPath.flatMap: (p, arts) =>
      if p.absolute.startsWith(pathpart)
      then arts
      else Nil

    ctx.fold(found): (cc, art) =>
      subconverter(
        art,
        anal
      ).convertSastSeq(art.sast, cc)
  }

  def handleInclude(ctx: Cta, directive: Directive) = {
    val attributes = directive.attributes
    attributes.arguments.headOption match
      case Some("code") =>
        article.doc.resolve(attributes.target) match
          case None => convertInlineSeq(List(directive), ctx)
          case Some(file) =>
            convertSast(
              Block(BCommand.Code, attributes, Fenced(Files.readString(file.absolute)))(directive.prov),
              ctx
            )

      case None =>
        if attributes.target.endsWith(".scim") then
          scribe.error(s"including by path no longer supported" + article.doc.reporter(directive))
          ctx.empty
        else
          anal.directory.findByLabel(attributes.target) match
            case None =>
              scribe.error(
                s"unknown include article ${attributes.target}" + article.doc.reporter(directive.prov)
              )
              ctx.empty
            case Some(article) =>
              subconverter(
                article.article,
                anal
              ).convertSastSeq(article.article.sast, ctx)

      case Some(other) =>
        scribe.error(s"unknown include type $other" + article.doc.reporter(directive.prov))
        ctx.empty
  }

end ProtoConverter
