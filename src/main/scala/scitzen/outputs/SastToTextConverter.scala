package scitzen.outputs

import de.rmgk.Chain
import scitzen.cli.ConversionAnalysis
import scitzen.generic.Document
import scitzen.sast.DCommand.{Include, Lookup}
import scitzen.sast.{
  Attribute, Attributes, BCommand, Block, Directive, Fenced, InlineText, ListItem, Paragraph, Parsed, Sast, Section,
  Slist, SpaceComment, Text
}

case class SastToTextConverter(
    doc: Document,
    anal: ConversionAnalysis,
    settings: Attributes,
) extends ProtoConverter[String, String](doc, anal, settings):

  override def subconverter(
      doc: Document,
      analysis: ConversionAnalysis,
      attr: Attributes
  ): ProtoConverter[String, String] =
    SastToTextConverter(doc, analysis, attr)

  override def convertBlock(ctx: Cta, block: Block): CtxCF =
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
          convertInlinesAsBlock(ctx, content.inl).map(r => Chain(r, ""))
        case Parsed(_, blockContent) => convertSastSeq(ctx, blockContent)
        case Fenced(text)            => ctx.retc(text)
        case SpaceComment(str)       => ctx.retc(str)

  override def convertSection(ctx: Cta, section: Section): CtxCF =
    convertInlineSeq(ctx, section.titleText.inl).mapc(inlinesAsToplevel)
  override def convertSlist(ctx: Cta, slist: Slist): CtxCF =
    val Slist(children) = slist
    ctx.fold(children): (ctx, child) =>
      child match
        case ListItem(_, Text(inl), None) =>
          convertInlineSeq(ctx, inl)
        case ListItem(_, text, Some(inner)) =>
          val tctx = convertInlineSeq(ctx, text.inl)
          tctx.data ++: convertSast(tctx, inner)
  override def inlineResToBlock(inl: Chain[String]): String  = inl.mkString("")
  override def inlinesAsToplevel(inl: Chain[String]): String = inl.mkString("")

  override def convertBlockDirective(ctx: Cta, directive: Directive): CtxCF =
    directive.command match
      case Include =>
        handleInclude(ctx, directive)
      case _ => convertDirective(ctx, directive)

  override def convertText(ctx: Cta, inlineText: InlineText): CtxInl = ctx.retc(inlineText.str)
  override def convertDirective(ctx: Cta, directive: Directive): CtxInl = directive.command match
    case Lookup =>
      handleLookup(directive) match
        case None =>
          ctx.empty
        case Some(res) =>
          convertInlineSeq(ctx, res.inl)

    case other => ctx.retc("")

  override def stringToInlineRes(str: String): String = str
