package scitzen.outputs

import de.rmgk.Chain
import scitzen.cli.ConversionAnalysis
import scitzen.project.ArticleRef
import scitzen.sast.DCommand.{Include, Lookup, Other}
import scitzen.sast.{
  Attribute, Attributes, BCommand, Block, Directive, Fenced, InlineText, ListItem, Paragraph, Parsed, Sast, Section,
  Slist, SpaceComment, Text
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
    val Block(command, attr, blockType) = block
    val keepBlock =
      command match
        case BCommand.If =>
          val res =
            settings.get(attr.target) match
              case Some(_) if attr.plain("equals").isEmpty =>
                true
              case Some(Attribute.Named(id, value)) =>
                attr.get("equals").forall(_ == value)
              case other => false
          if attr.get("not").isDefined then !res
          else res
        case _ => true

    if !keepBlock then ctx.empty
    else
      blockType match
        case Paragraph(content) =>
          convertInlinesCombined(ctx, content.inl).map(r => Chain(r, "\n\n"))
        case Parsed(_, blockContent) => convertSastSeq(ctx, blockContent)
        case Fenced(text)            => ctx.retc(text)
        case SpaceComment(str)       => ctx.retc("")

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
