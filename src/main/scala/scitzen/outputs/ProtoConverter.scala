package scitzen.outputs

import de.rmgk.Chain
import de.rmgk.logging.Loggable
import scitzen.cli.ConversionAnalysis
import scitzen.compat.Logging.cli
import scitzen.contexts.{ConversionContext, FileDependency}
import scitzen.extern.ImageTarget
import scitzen.generic.{Article, ArticleRef, ProjectPath, TitledArticle}
import scitzen.sast.*
import scitzen.sast.Attribute.Named

import java.nio.file.Files

abstract class ProtoConverter[BlockRes, InlineRes](
    articleRef: ArticleRef,
    anal: ConversionAnalysis,
    settings: Attributes,
    outputDirectory: ProjectPath,
):

  def doc      = articleRef.document
  val project  = anal.project
  val reporter = doc.reporter

  type CtxCF  = ConversionContext[Chain[BlockRes]]
  type CtxInl = ConversionContext[Chain[InlineRes]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[?]

  val videoEndings = List(".mp4", ".mkv", ".webm")

  def subconverter(
      articleRef: ArticleRef,
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
      settings.get(id).orElse(directive.attributes.get("default")).collect:
        case Named(_, value) => value
    if res.isEmpty then
      cli.warn(s"unknown name ${id}" + reporter(directive))
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

  def convertImage(ctx: Cta, directive: Directive, imageTarget: ImageTarget)(cont: Ctx[FileDependency] => CtxInl): CtxInl =
    doc.resolve(directive.attributes.target) match
      case Some(path) if Files.exists(path.absolute) =>
        if !imageTarget.requiresConversion(path)
        then
          val dep = FileDependency(path, path, project.htmlPaths.relativizeImage(path), outputDirectory)
          cont(ctx.ret(dep).requireInOutput(dep))
        else
          anal.project.imagePaths.lookup(imageTarget).predictTarget(path) match
            case None =>
              cli.warn(s"cannot convert to ${imageTarget.preferredFormat} (or ${imageTarget.alternative.mkString(", ")})", directive)
              ctx.retc(stringToInlineRes(directiveString(directive)))
            case Some(target) =>
              val dep = FileDependency(target, path, project.htmlPaths.relativizeImage(target), outputDirectory)
              cont(ctx.ret(dep).requireInOutput(dep))
      case other =>
        cli.warn(s"could not find path", directive)
        ctx.retc(stringToInlineRes(directiveString(directive)))

  def stringToInlineRes(str: String): InlineRes

  def handleArticleQuery(directive: Directive): Iterable[TitledArticle] =
    val pathpart =
      directive.attributes.get("prefix").map(p => doc.path.absolute.resolveSibling(p.text.plainString).normalize())
    val tags =
      directive.attributes.plain("tags").getOrElse("").split(',').map(_.trim).filter(_.nonEmpty)
    val it: Iterable[TitledArticle] = anal.directory.titled.view
    val it2 = pathpart match
      case Some(f) => it.filter(_.article.doc.path.absolute.startsWith(f))
      case None    => it
    val it3 =
      if tags.nonEmpty
      then
        it2.filter: titled =>
          tags.exists(titled.header.tags.contains)
      else it2
    it3.filter(_.article.ref != articleRef)

  def handleAggregate(ctx: Cta, directive: Directive): ConversionContext[Chain[BlockRes]] = {
    val it = handleArticleQuery(directive)
    ctx.fold(it): (cc, titled) =>
      subconverter(titled.article.ref, settings).convertSastSeq(cc, titled.article.sast)
  }

  def handleIndex(ctx: Cta, directive: Directive): ConversionContext[Chain[BlockRes]] = {
    val it   = handleArticleQuery(directive)
    val sast = GenIndexPage.makeIndex(it.toList, project)
    convertSastSeq(ctx, sast)
  }

  given Loggable[Prov] with
    override def normal(prov: Prov): String = doc.reporter.apply(prov)
  given Loggable[Directive] with
    override def normal(dir: Directive): String =
      s"⸢${directiveString(dir)}⸥${doc.reporter(dir)}"
  given Loggable[Block] with
    override def normal(block: Block): String =
      s"${block.command}${doc.reporter(block.prov)}"

  def directiveString(directive: Directive) = SastToScimConverter(anal.bib).macroToScim(directive)

  def handleInclude(ctx: Cta, directive: Directive): Ctx[Chain[BlockRes | InlineRes]] = {
    val attributes = directive.attributes
    attributes.text.plainString match
      case "code" =>
        doc.resolve(attributes.target) match
          case None => convertInlineSeq(ctx, List(directive))
          case Some(file) =>
            convertSast(ctx, Block(BCommand.Code, attributes, Fenced(Files.readString(file.absolute)))(directive.prov))

      case other =>
        val resolution: Seq[Article] = if attributes.target.endsWith(".scim") then
          doc.resolve(attributes.target).flatMap(anal.directory.byPath.get).getOrElse(Nil)
        else
          anal.directory.findByLabel(attributes.target).map(_.article).toList
        if resolution.nonEmpty
        then
          (ctx.fold(resolution): (ctx, article) =>
            subconverter(article.ref, settings).convertSastSeq(ctx, article.sast).map: res =>
              ctx.data ++ res): CtxCF
        else
          cli.warn(
            s"unknown include article ${attributes.target}",
            directive.prov
          )
          ctx.empty: CtxCF

  }

end ProtoConverter
