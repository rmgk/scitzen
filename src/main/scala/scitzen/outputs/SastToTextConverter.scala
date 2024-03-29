package scitzen.outputs

import de.rmgk.Chain
import scitzen.cli.ConversionAnalysis
import scitzen.project.ArticleRef
import scitzen.sast.DCommand.{Include, Lookup, Other}
import scitzen.sast.{
  Attribute, Attributes, BCommand, Block, FusedDefinitionItem, Directive, Fenced, InlineText, FusedListItem, Paragraph,
  FusedDelimited, Sast, FusedDefinitions, Section, FusedList, SpaceComment, Text
}

class SastToTextConverter(
    articleRef: ::[ArticleRef],
    anal: ConversionAnalysis,
    settings: Attributes,
) extends ProtoConverter[String, String](articleRef, anal, settings):

  override def subconverter(
      aref: ArticleRef,
      attr: Attributes
  ): ProtoConverter[String, String] =
    SastToTextConverter(::(aref, articleRef), anal, attr)

  override def convertDelimited(ctx: Cta, block: FusedDelimited): CtxCF =
    convertBlock(ctx, block.delimiter.command, block.delimiter.attributes, block)
  override def convertFenced(ctx: Cta, block: Fenced): CtxCF = convertBlock(ctx, block.command, block.attributes, block)
  def convertBlock(ctx: Cta, command: BCommand, attributes: Attributes, block: Block): CtxCF =
    val keepBlock =
      command match
        case BCommand.If =>
          val res =
            settings.get(attributes.target) match
              case Some(_) if attributes.plain("equals").isEmpty =>
                true
              case Some(Attribute(id, text)) if id.nonEmpty =>
                attributes.get("equals").forall(_.text == text)
              case other => false
          if attributes.get("not").isDefined then !res
          else res
        case _ => true

    if !keepBlock then ctx.empty
    else
      block match
        case FusedDelimited(_, _, blockContent) => convertSastSeq(ctx, blockContent)
        case Fenced(_, _, text, _)           => ctx.retc(text)

  override def convertParagraph(ctx: Cta, paragraph: Paragraph): CtxCF =
    convertInlinesCombined(ctx, paragraph.inlines).map(r => Chain(r, "\n\n"))

  override def convertSection(ctx: Cta, section: Section): CtxCF =
    convertInlineSeq(ctx, section.titleText.inl).mapc(inlinesAsToplevel)
  override def convertSlist(ctx: Cta, slist: FusedList): CtxCF =
    ctx.fold(slist.items): (ctx, child) =>
      child match
        case fli: FusedListItem =>
          convertInlineSeq(ctx, fli.inlines)

  override def convertDefinitionList(ctx: Cta, deflist: FusedDefinitions): CtxCF =
    ctx.fold(deflist.items): (ctx, child) =>
      child match
        case fdi: FusedDefinitionItem =>
          val tctx = convertInlineSeq(ctx, fdi.text.inl)
          tctx.data ++: convertSastSeq(tctx, fdi.content)

  override def inlineResToBlock(inl: Chain[String]): String  = inl.mkString("")
  override def inlinesAsToplevel(inl: Chain[String]): String = inl.mkString("")

  override def convertBlockDirective(ctx: Cta, directive: Directive): CtxCF =
    directive.command match
      case Include =>
        handleInclude(ctx, directive)
      case _ => convertInlineDirective(ctx, directive)

  override def convertInlineText(ctx: Cta, inlineText: InlineText): CtxInl = ctx.retc(inlineText.str)
  override def convertInlineDirective(ctx: Cta, directive: Directive): CtxInl = directive.command match
    case Lookup =>
      handleLookup(directive) match
        case None =>
          ctx.empty
        case Some(res) =>
          convertInlineSeq(ctx, res.inl)
    case Other("path") =>
      val res = handlePath(ctx, directive)
      res.data match
        case Some(relative) =>
          val path = project.root.resolve(relative)
          res.retc(path.toString)
        case None => res.retc("")
    case other => ctx.retc("")

  override def stringToInlineRes(str: String): String = str
