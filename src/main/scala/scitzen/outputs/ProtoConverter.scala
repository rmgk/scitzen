package scitzen.outputs

import de.rmgk.Chain
import scitzen.cli.ConversionAnalysis
import scitzen.compat.Logging.scribe
import scitzen.contexts.ConversionContext
import scitzen.extern.ImageTarget
import scitzen.generic.{Article, Document, ProjectPath}
import scitzen.sast.*

import java.nio.file.Files

abstract class ProtoConverter[BlockRes, InlineRes](
    doc: Document,
    anal: ConversionAnalysis,
    combinedAttributes: Attributes,
):

  val project  = doc.path.project
  val reporter = doc.reporter

  val hardNewlines = !combinedAttributes.named.get("style").exists(_.contains("article"))

  type CtxCF  = ConversionContext[Chain[BlockRes]]
  type CtxInl = ConversionContext[Chain[InlineRes]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[?]

  val videoEndings = List(".mp4", ".mkv", ".webm")

  def subconverter(
      doc: Document,
      analysis: ConversionAnalysis,
      attr: Attributes
  ): ProtoConverter[BlockRes, InlineRes]

  // todo … this implies that we need some way to handle such named cases systematically
  // currently, the default could not be named for example, which seems weird
  // basically, needing a rendering of an `Attribute` – or probably better a systematic flattening of an attribute to a Text
  // I think this might warrant an experiment if we want to:
  // • embrace the dynamic nature of `Attributes`
  // • properly type them during parsing
  def handleLookup(directive: Directive): Option[Text] =
    val id = directive.attributes.target
    val res =
      combinedAttributes.nestedMap.get(id).map: attr =>
        attr.targetT
      .orElse:
        combinedAttributes.named.get(id).map: str =>
          Text.of(str)
      .orElse(directive.attributes.named.get("default").map(Text.of))
    if res.isEmpty then
      scribe.warn(s"unknown name ${id}" + reporter(directive))
    res

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

  def convertBlock(block: Block, ctx: Cta): CtxCF
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
    val target = anal.image.lookup(doc.resolve(directive.attributes.target).get, imageTarget)
    if !Files.exists(target.absolute)
    then ctx.retc(warn(s"could not find path", directive))
    else cont(target)

  def stringToInlineRes(str: String): InlineRes

  def warn(msg: String, im: Directive): InlineRes =
    val macroStr = SastToScimConverter(anal.bib).macroToScim(im)
    scribe.warn(s"$msg: ⸢$macroStr⸥${doc.reporter(im)}")
    stringToInlineRes(macroStr)

  def handleAggregate(ctx: Cta, directive: Directive): ConversionContext[Chain[BlockRes]] = {
    val pathpart = doc.path.directory.resolve(directive.attributes.target).normalize()
    val found = anal.directory.byPath.flatMap: (p, arts) =>
      if p.absolute.startsWith(pathpart)
      then arts
      else Nil

    ctx.fold(found): (cc, art) =>
      subconverter(art.doc, anal, combinedAttributes).convertSastSeq(art.sast, cc)
  }

  def handleInclude(ctx: Cta, directive: Directive) = {
    val attributes = directive.attributes
    attributes.arguments.headOption match
      case Some("code") =>
        doc.resolve(attributes.target) match
          case None => convertInlineSeq(List(directive), ctx)
          case Some(file) =>
            convertSast(
              Block(BCommand.Code, attributes, Fenced(Files.readString(file.absolute)))(directive.prov),
              ctx
            )

      case None =>
        val resolution: Option[Article] = if attributes.target.endsWith(".scim") then
          doc.resolve(attributes.target).flatMap(anal.directory.byPath.get).flatMap(_.headOption)
        else
          anal.directory.findByLabel(attributes.target).map(_.article)
        resolution match
          case None =>
            scribe.error(
              s"unknown include article ${attributes.target}" + doc.reporter(directive.prov)
            )
            ctx.empty
          case Some(article) =>
            subconverter(article.doc, anal, combinedAttributes).convertSastSeq(article.sast, ctx)

      case Some(other) =>
        scribe.error(s"unknown include type $other" + doc.reporter(directive.prov))
        ctx.empty
  }

end ProtoConverter
