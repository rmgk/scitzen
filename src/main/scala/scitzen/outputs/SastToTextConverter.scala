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

  override def convertBlock(ctx: Cta, block: Block): CtxCF =
    val keepBlock =
      block.command match
        case BCommand.If =>
          val res =
            settings.get(block.attributes.target) match
              case Some(_) if block.attributes.plain("equals").isEmpty =>
                true
              case Some(Attribute.Named(id, value)) =>
                block.attributes.get("equals").forall(_ == value)
              case other => false
          if block.attributes.get("not").isDefined then !res
          else res
        case _ => true

    if !keepBlock then ctx.empty
    else
      block match
        case FusedDelimited(_, blockContent)  => convertSastSeq(ctx, blockContent)
        case Fenced(_, _, text, _, _) => ctx.retc(text)

  override def convertParagraph(ctx: Cta, paragraph: Paragraph): CtxCF =
    convertInlinesCombined(ctx, paragraph.inlines).map(r => Chain(r, "\n\n"))

  override def convertSection(ctx: Cta, section: Section): CtxCF =
    convertInlineSeq(ctx, section.titleText.inl).mapc(inlinesAsToplevel)
  override def convertSlist(ctx: Cta, slist: FusedList): CtxCF =
    ctx.fold(slist.items): (ctx, child) =>
      child match
        case fli: FusedListItem =>
          convertInlineSeq(ctx, fli.paragraph.inlines)

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
        case Some(path) =>
          res.retc(path.toString)
        case None => res.retc("")
    case other => ctx.retc("")

  override def stringToInlineRes(str: String): String = str
