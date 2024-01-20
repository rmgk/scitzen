package scitzen.outputs

import de.rmgk.Chain
import de.rmgk.logging.Loggable
import scitzen.cli.ConversionAnalysis
import scitzen.compat.Logging.cli
import scitzen.contexts.{ConversionContext, FileDependency}
import scitzen.project.{ArticleRef, ProjectPath, References, TitledArticle}
import scitzen.resources.ImageTarget
import scitzen.sast.*

import java.nio.file.{Files, Path}

abstract class ProtoConverter[BlockRes, InlineRes](
    articleRef: ::[ArticleRef],
    anal: ConversionAnalysis,
    settings: Attributes,
):

  def doc      = articleRef.head.document
  val project  = anal.project
  val reporter = doc.reporter

  type CtxCF  = ConversionContext[Chain[BlockRes]]
  type CtxInl = ConversionContext[Chain[InlineRes]]
  type Ctx[T] = ConversionContext[T]
  type Cta    = Ctx[?]

  val videoEndings = List(".mp4", ".mkv", ".webm")

  def subconverter(
      aref: ArticleRef,
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
      settings.get(id).orElse(directive.attributes.get("default")).map(_.text)
    if res.isEmpty then
      cli.warn(s"unknown name ${id}" + reporter(directive))
    res

  def convertSastSeq(ctx: Cta, b: Iterable[Sast]): CtxCF = ctx.fold(b)((ctx, sast) => convertSast(ctx, sast))

  def convertSast(ctx: Cta, singleSast: Sast): CtxCF =
    singleSast match
      case _: SpaceComment      => ctx.empty
      case paragraph: Paragraph => convertParagraph(ctx, paragraph)
      case block: Fenced =>
        if block.command == BCommand.Convert then
          convertSastSeq(ctx, anal.block.substitute(block))
        else convertFenced(ctx, block)
      case block: FusedDelimited =>
        if block.delimiter.command == BCommand.Convert then
          convertSastSeq(ctx, anal.block.substitute(block))
        else convertDelimited(ctx, block)
      case directive: Directive   => convertBlockDirective(ctx, directive)
      case section: Section       => convertSection(ctx, section)
      case slist: FusedList       => convertSlist(ctx, slist)
      case sdef: FusedDefinitions => convertDefinitionList(ctx, sdef)

  def convertDelimited(ctx: Cta, block: FusedDelimited): CtxCF
  def convertFenced(ctx: Cta, block: Fenced): CtxCF
  def convertBlockDirective(ctx: Cta, directive: Directive): CtxCF =
    convertInlineDirective(ctx, directive).map(v => Chain(inlinesAsToplevel(v)))
  def convertSection(ctx: Cta, section: Section): CtxCF
  def convertParagraph(ctx: Cta, paragraph: Paragraph): CtxCF
  def convertSlist(ctx: Cta, slist: FusedList): CtxCF
  def convertDefinitionList(ctx: Cta, deflist: FusedDefinitions): CtxCF

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

  def convertImage(
      ctx: Cta,
      directive: Directive,
      imageTarget: ImageTarget
  )(cont: Ctx[FileDependency] => CtxInl): CtxInl =
    References.resolveResource(project, doc, directive.attributes.target) match
      case Seq(path) if Files.exists(path.absolute) =>
        if !imageTarget.requiresConversion(path)
        then
          val dep = FileDependency(path, path, project.imagePaths.relativizeImage(path))
          cont(ctx.ret(dep).requireInOutput(dep))
        else
          anal.project.imagePaths.lookup(imageTarget).predictTarget(path) match
            case None =>
              cli.warn(s"cannot convert to ${imageTarget.choices}", directive)
              ctx.retc(stringToInlineRes(directiveString(directive)))
            case Some(target) =>
              val dep = FileDependency(target, path, project.imagePaths.relativizeImage(target))
              cont(ctx.ret(dep).requireInOutput(dep))
      case other =>
        cli.warn(s"could not find path", directive)
        ctx.retc(stringToInlineRes(directiveString(directive)))

  def stringToInlineRes(str: String): InlineRes

  def handleArticleQuery(directive: Directive): Iterable[TitledArticle] =
    val pathpart =
      directive.attributes.get("prefix").map(p => doc.path.absolute.resolveSibling(p.raw).normalize())
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
    val includes = anal.directory.includesFix(articleRef.last)
    it3.filter(v => !includes(v.article.ref))

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
  given Loggable[FusedDelimited] with
    override def normal(block: FusedDelimited): String =
      s"${block.delimiter.command}${doc.reporter(block.delimiter.meta.prov)}"

  def directiveString(directive: Directive) = SastToScimConverter(anal.bib).directive(directive)

  def handleInclude(ctx: Cta, directive: Directive): Ctx[Chain[BlockRes | InlineRes]] = {
    val attributes = directive.attributes
    attributes.specifier match
      case Some("code") =>
        doc.resolve(attributes.target) match
          case None => convertInlineSeq(ctx, List(directive))
          case Some(file) =>
            convertSast(ctx, Fenced(BCommand.Code, attributes, Files.readString(file.absolute), directive.meta))

      case other =>
        val resolution = References.resolve(directive, document = doc, directory = anal.directory).map: sr =>
          anal.directory.snippetByRef(sr.articleRef)
        if resolution.nonEmpty
        then
          (ctx.fold(resolution): (ctx, article) =>
            subconverter(article.ref, settings).convertSastSeq(ctx, article.sast).map: res =>
              ctx.data ++ res): CtxCF
        else
          cli.warn(
            s"unknown include article ${attributes.target}",
            directive.meta.prov
          )
          ctx.empty: CtxCF

  }

  def handlePath(ctx: Cta, directive: Directive): Ctx[Option[Path]] = {
    val search = Path.of(directive.attributes.target)
    project.projectFiles.find: p =>
      p.projectAbsolute.endsWith(search)
    match
      case None => ctx.ret(None)
      case Some(pp) =>
        val relative = anal.project.root.relativize(pp.absolute)
        ctx.requireInOutput(FileDependency(pp, pp, relative)).ret(Some(relative))
  }

end ProtoConverter
