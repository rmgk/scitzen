package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files.File
import com.monovore.decline.{Command, Opts}
import scitzen.parser.{Attribute, AttributeBlock, Block, BlockContent, Inline, InlineQuote, InlineText, ListBlock, ListItem, Macro, NormalBlock, Prov, SectionTitle, WhitespaceBlock}
import scitzen.semantics.{Sast, SastConverter}
import scitzen.semantics.Sast.{AttributeDef, AttributedBlock, MacroBlock, ParsedBlock, RawBlock, Section, Slist, SlistItem, Text}
import upickle.default.macroW
import upickle.default.Writer

object JsonSast {

  implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8

  implicit val MacroEncoder          : Writer[Macro]           = macroW
  implicit val InlineTextEncoder     : Writer[InlineText]      = macroW
  implicit val ProvEncoder           : Writer[Prov]            = macroW
  implicit val AttributeBlockEncoder : Writer[AttributeBlock]  = macroW
  implicit val ListBlockEncoder      : Writer[ListBlock]       = macroW
  implicit val NormalBlockEncoder    : Writer[NormalBlock]     = macroW
  implicit val SectionTitleEncoder   : Writer[SectionTitle]    = macroW
  implicit val ListItemEncoder       : Writer[ListItem]        = macroW
  implicit val WhitespaceBlockEncoder: Writer[WhitespaceBlock] = macroW
  implicit val BlockContentEncoder   : Writer[BlockContent]    = macroW
  implicit val InlineQuoteEncoder    : Writer[InlineQuote]     = macroW
  implicit val InlineEncoder         : Writer[Inline]          = macroW
  implicit val BlockEncoder          : Writer[Block]           = macroW
  implicit val AttributeEncoder      : Writer[Attribute]       = macroW
  implicit val SlistEncoder          : Writer[Slist]           = macroW
  implicit val SlistItemEncoder      : Writer[SlistItem]       = macroW
  implicit val TextEncoder           : Writer[Text]            = macroW
  implicit val SectionEncoder        : Writer[Section]         = macroW
  implicit val MacroBlockEncoder     : Writer[MacroBlock]      = macroW
  implicit val RawBlockEncoder       : Writer[RawBlock]        = macroW
  implicit val ParsedBlockEncoder    : Writer[ParsedBlock]     = macroW
  implicit val AttributedBlockEncoder: Writer[AttributedBlock] = macroW
  implicit val AttributeDefEncoder   : Writer[AttributeDef]    = macroW
  implicit val SastEncoder           : Writer[Sast]            = macroW


  val command: Command[Unit] = Command(name = "json",
                                       header = "Convert Scim to Json") {

    Opts.arguments[Path](metavar = "paths").map {
      _.map(File(_))
       .filter(_.isRegularFile)
       .foreach { file =>
         val content = file.contentAsString
         val sast = new SastConverter(Convert.includeBelow(file)).documentString(content)
         val target = file.sibling(file.name + ".json")
         val json = upickle.default.write(sast, indent = 2)
         target.write(json)
       }
    }

  }

}
