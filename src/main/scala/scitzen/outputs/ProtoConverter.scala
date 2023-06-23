package scitzen.outputs

import de.rmgk.Chain
import scitzen.cli.ConversionAnalysis
import scitzen.compat.Logging.scribe
import scitzen.contexts.ConversionContext
import scitzen.extern.ImageTarget
import scitzen.generic.{Article, Document, ProjectPath}
import scitzen.sast.*
import scitzen.sast.Attribute.Normal

import java.nio.file.Files

abstract class ProtoConverter[BlockRes, InlineRes](
    doc: Document,
    anal: ConversionAnalysis,
    combinedAttributes: Attributes,
):

  val project  = doc.path.project
  val reporter = doc.reporter

  val hardNewlines = !combinedAttributes.plain("style").exists(_.contains("article"))

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
      combinedAttributes.get(id).orElse(directive.attributes.get("default")).collect:
        case Normal(_, value) => value
    if res.isEmpty then
      scribe.warn(s"unknown name ${id}" + reporter(directive))
    res

  def convertSastSeq(ctx: Cta, b: Iterable[Sast]): CtxCF = ctx.fold(b)((ctx, sast) => convertSast(ctx, sast))

  def convertSast(ctx: Cta, singleSast: Sast): CtxCF =
    singleSast match
      case block: Block =>
        if block.command == BCommand.Convert then
          convertSastSeq(ctx, anal.block.substitute(block))
        else convertBlock(ctx, block)
      case directive: Directive => convertBlockDirective(ctx, directive)
      case section: Section     => convertSection(ctx, section)
      case slist: Slist         => convertSlist(ctx, slist)

  def convertBlock(ctx: Cta, block: Block): CtxCF
  def convertBlockDirective(ctx: Cta, directive: Directive): CtxCF =
    convertInlineDirective(ctx, directive).map(v => Chain(inlinesAsToplevel(v)))
  def convertSection(ctx: Cta, section: Section): CtxCF
  def convertSlist(ctx: Cta, slist: Slist): CtxCF

  def convertInlinesCombined(ctx: Cta, inlines: Iterable[Inline]): Ctx[BlockRes] =
    convertInlineSeq(ctx, inlines).map(v => inlineResToBlock(v))

  def inlineResToBlock(inl: Chain[InlineRes]): BlockRes
  def inlinesAsToplevel(inl: Chain[InlineRes]): BlockRes

  def convertInlineSeq(ctx: Cta, inlines: Iterable[Inline]): CtxInl =
    ctx.fold(inlines) { (ctx, inline) => convertInline(ctx, inline) }

  def convertInline(ctx: Cta, inlineSast: Inline): CtxInl =
    inlineSast match
      case inlineText: InlineText => convertInlineText(ctx, inlineText)
      case directive: Directive   => convertInlineDirective(ctx, directive)

  def convertInlineText(ctx: Cta, inlineText: InlineText): CtxInl
  def convertInlineDirective(ctx: Cta, directive: Directive): CtxInl

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
      subconverter(art.doc, anal, combinedAttributes).convertSastSeq(cc, art.sast)
  }

  def handleInclude(ctx: Cta, directive: Directive) = {
    val attributes = directive.attributes
    attributes.text.plainString match
      case "code" =>
        doc.resolve(attributes.target) match
          case None => convertInlineSeq(ctx, List(directive))
          case Some(file) =>
            convertSast(ctx, Block(BCommand.Code, attributes, Fenced(Files.readString(file.absolute)))(directive.prov))

      case other =>
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
            subconverter(article.doc, anal, combinedAttributes).convertSastSeq(ctx, article.sast)
  }

end ProtoConverter
