package scitzen.outputs

import de.rmgk.Chain
import scitzen.cli.ConversionAnalysis
import scitzen.sast.{
  Attribute, Attributes, BCommand, Block, Directive, Fenced, InlineText, ListItem, Paragraph, Parsed, Sast, Section,
  Slist, SpaceComment, Text
}
import scitzen.generic.Article
import scitzen.sast.DCommand.{Include, Lookup}

case class SastToTextConverter(
    article: Article,
    anal: ConversionAnalysis,
    settings: Attributes,
) extends ProtoConverter[String, String](article, anal, settings):

  override def subconverter(
      article: Article,
      analysis: ConversionAnalysis,
      attr: Attributes
  ): ProtoConverter[String, String] =
    new SastToTextConverter(article, analysis, attr)
  override def convertBlock(block: Block, ctx: Cta): CtxCF =
    val Block(command, attr, blockType) = block
    val keepBlock =
      command match
        case BCommand.If =>
          val res =
            settings.attrMap.get(attr.target) match
              case Some(_) if !attr.named.contains("equals") =>
                true
              case Some(Attribute.Plain(id, value)) =>
                attr.named.get("equals").forall(_ == value)
              case other => false
          if attr.named.contains("not") then !res
          else res
        case _ => true

    if !keepBlock then ctx.empty
    else
      blockType match
        case Paragraph(content) =>
          convertInlinesAsBlock(content.inl, ctx).map(r => Chain(r, ""))
        case Parsed(_, blockContent) => convertSastSeq(blockContent, ctx)
        case Fenced(text)            => ctx.retc(text)
        case SpaceComment(str)       => ctx.retc(str)

  override def convertSection(section: Section, ctx: Cta): CtxCF =
    convertInlineSeq(section.titleText.inl, ctx).mapc(inlinesAsToplevel)
  override def convertSlist(slist: Slist, ctx: Cta): CtxCF =
    val Slist(children) = slist
    ctx.fold(children): (ctx, child) =>
      child match
        case ListItem(_, Text(inl), None) =>
          convertInlineSeq(inl, ctx)
        case ListItem(_, text, Some(inner)) =>
          val tctx = convertInlineSeq(text.inl, ctx)
          tctx.data ++: convertSast(inner, tctx)
  override def inlineResToBlock(inl: Chain[String]): String  = inl.mkString("")
  override def inlinesAsToplevel(inl: Chain[String]): String = inl.mkString("")

  override def convertDirective(directive: Directive, ctx: Cta): CtxCF =
    directive.command match
      case Include =>
        handleInclude(ctx, directive)

      case _ => convertInlineDirective(directive, ctx)
  override def convertInlineText(inlineText: InlineText, ctx: Cta): CtxInl = ctx.retc(inlineText.str)
  override def convertInlineDirective(directive: Directive, ctx: Cta): CtxInl = directive.command match
    case Lookup =>
      println(s"loookup of ${directive.attributes.target}")
      val res = handleLookup(directive) match
        case None =>
          println(s"is empty")
          ctx.empty
        case Some(res) =>
          convertInlineSeq(res.inl, ctx)
      println(s"returned $res")
      res

    case other => ctx.retc("")

  override def stringToInlineRes(str: String): String = str
  override def addDetail(ctx: CtxCF): CtxCF           = ctx
