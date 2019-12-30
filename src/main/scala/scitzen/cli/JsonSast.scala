package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files.File
import com.monovore.decline.{Command, Opts}
import scitzen.generic.Sast._
import scitzen.generic.{Sast, SastConverter}
import scitzen.parser._
import upickle.default.{Writer, macroW}

object JsonSast {

  implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8

  implicit val AttributesEncoder     : Writer[Attributes]      = macroW
  implicit val ProvEncoder           : Writer[Prov]            = macroW
  implicit val MacroEncoder          : Writer[Macro]           = macroW
  implicit val InlineTextEncoder     : Writer[InlineText]      = macroW
  implicit val ListBlockEncoder      : Writer[ListBlock]       = macroW
  implicit val NormalBlockEncoder    : Writer[NormalBlock]     = macroW
  implicit val SectionTitleEncoder   : Writer[SectionTitle]    = macroW
  implicit val ListItemEncoder       : Writer[ListItem]        = macroW
  implicit val WhitespaceBlockEncoder: Writer[WhitespaceBlock] = macroW
  implicit val BlockContentEncoder   : Writer[BlockContent]    = macroW
  implicit val InlineEncoder         : Writer[Inline]          = macroW
  implicit val BlockEncoder          : Writer[Block]           = macroW
  implicit val AttributeEncoder      : Writer[Attribute]       = macroW
  implicit val SlistEncoder          : Writer[Slist]           = macroW
  implicit val SlistItemEncoder      : Writer[SlistItem]       = macroW
  implicit val TextEncoder           : Writer[Text]            = macroW
  implicit val SectionEncoder        : Writer[Section]         = macroW
  implicit val MacroBlockEncoder     : Writer[SMacro]          = macroW
  implicit val RawBlockEncoder       : Writer[Fenced]          = macroW
  implicit val ParsedBlockEncoder    : Writer[Parsed]          = macroW
  implicit val ParagraphEncoder      : Writer[Paragraph]       = macroW
  implicit val SpaceCommendEncoder   : Writer[SpaceComment]    = macroW
  implicit val SBlockTypeEncoder     : Writer[BlockType]       = macroW
  implicit val AttributedBlockEncoder: Writer[SBlock]          = macroW
  implicit val SastEncoder           : Writer[Sast]            = macroW


  val command: Command[Unit] = Command(name = "json",
                                       header = "Convert Scim to Json") {

    Opts.arguments[Path](metavar = "paths").map {
      _.map(File(_))
       .filter(_.isRegularFile)
       .foreach { file =>
         val content = file.contentAsString
         val sast    = SastConverter().documentString(content, Prov(0, content.length))
         val target  = file.sibling(file.name + ".json")
         val json    = upickle.default.write(sast, indent = 2)
         target.write(json)
       }
    }

  }

}
