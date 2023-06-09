package scitzen.outputs

import de.rmgk.Chain
import de.rmgk.scip.Scx
import scitzen.bibliography.BibDB
import scitzen.parser.{AttributeDeparser, AttributesParser, Parse}
import scitzen.sast.*
import scitzen.sast.Attribute.{Nested, Plain, Positional}
import scitzen.sast.DCommand.{BibQuery, Comment}

import java.nio.charset.StandardCharsets
import scala.collection.immutable.ArraySeq
import scala.util.Try
import scala.util.matching.Regex

class SastToScimConverter(bibDB: BibDB):

  val attributeConverter = AttributesToScim(bibDB)

  def attributesToScim(
      attributes: Attributes,
      spacy: Boolean,
      force: Boolean,
      light: Boolean = false
  ): Chain[String] =
    if !force && attributes.raw.isEmpty then Chain.nil
    else Chain(AttributesToScim(bibDB).convert(attributes, spacy, force, light))

  def toScimS(b: Seq[Sast]): Chain[String] =
    Chain.from(b).flatMap(toScim)

  def toScim(sast: Sast): Chain[String] =
    sast match
      case Section(title, prefix, attributes) =>
        (prefix + " " + inlineToScim(title.inl)) +:
        attributesToScim(attributes, spacy = true, force = false, light = true)

      case Slist(children) => Chain.from(children).flatMap {
          case ListItem(marker, inner, None) =>
            Chain(marker + inlineToScim(inner.inl))

          case ListItem(marker, Text(inl), Some(rest)) =>
            Chain(
              s"$marker" + inlineToScim(inl) + (if rest.isInstanceOf[Slist] then "" else ":"),
              toScim(rest).iterator.mkString("\n").stripTrailing
            )
        }

      case mcro: Directive => Chain(macroToScim(mcro))

      case tlb: Block => convertBlock(tlb)

  def stripLastEnd(strings: Chain[String]): Chain[String] =
    Chain.from(
      (strings.iterator.toList.reverse match
        case Nil => Nil
        case head :: tail =>
          val stripped = head.stripTrailing()
          if stripped.isEmpty then tail else stripped :: tail
      ).reverse
    )

  def addIndent(lines: String, delimiter: String): String =
    lines.linesWithSeparators.map { line =>
      if line == "\n" then line
      else delimiter + line
    }.mkString

  def convertBlock(sb: Block): Chain[String] =
    val (remattr, command: String) = sb.attributes.raw.headOption match
      case Some(Positional(_, value)) => (sb.attributes.copy(raw = sb.attributes.raw.drop(1)), value)
      case _                                => (sb.attributes, "")
    sb.content match

      case Paragraph(content) =>
        val attrres = attributesToScim(sb.attributes, spacy = false, force = false)
        attrres :+ inlineToScim(content.inl) :+ ""

      case Parsed(delimiter, blockContent) =>
        val content = toScimS(blockContent)
        delimiter.charAt(0) match
          case ':' =>
            Chain(
              "::" + command +
              AttributesToScim(bibDB).convert(remattr, force = false, spacy = false),
              content.map(addIndent(_, "\t")).iterator.mkString("\n").stripTrailing(),
              "::"
            )
          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case ' ' | '\t' =>
            stripLastEnd(content.map(addIndent(_, delimiter)))

      case SpaceComment(text) =>
        Chain.from(ArraySeq.unsafeWrapArray(
          text.stripLineEnd.split("\\n", -1).map(_.trim)
        ))
      case Fenced(text) =>
        val delimiter = "``"
        Chain(
          delimiter + command + AttributesToScim(bibDB).convert(remattr, spacy = false, force = false),
          addIndent(text, "\t"),
          delimiter
        )

  def macroToScim(mcro: Directive, spacy: Boolean = false): String =
    mcro match
      case Directive(Comment, attributes) => s":%${attributes.target}"
      case Directive(BibQuery, _) => macroToScim(bibDB.convert(mcro))
      case _ =>
        s":${DCommand.printMacroCommand(mcro.command)}${attributeConverter.convert(mcro.attributes, spacy, force = true)}"

  def inlineToScim(inners: Seq[Inline]): String =
    inners.map {
      case InlineText(str) => str
      case m: Directive    => macroToScim(m)
    }.mkString("")

class AttributesToScim(bibDB: BibDB):

  def encodeText(text: Text): String =
    val value = SastToScimConverter(bibDB).inlineToScim(text.inl)
    AttributeDeparser.quote(
      value,
      {
        case `text` => true
        case other  => false
      }
    )
  def encodeString(value: String): String =
    AttributeDeparser.quote(
      value,
      {
        case Text(Nil) if value.isEmpty     => true
        case Text(Seq(InlineText(`value`))) => true
        case other                          => false
      }
    )

  def convert(attributes: Attributes, spacy: Boolean, force: Boolean, light: Boolean = false): String =
    if !force && attributes.raw.isEmpty then return ""
    val keylen = (attributes.raw.map {
      case Plain(id, _)  => id.length
      case Nested(id, _) => id.length
      case other         => 0
    }).maxOption.getOrElse(0)
    val pairs = attributes.raw.map {
      case Positional(v, _) => encodeText(v)
      case Plain(k, v) =>
        val spaces = " " * math.max(keylen - k.length, 0)
        if spacy then s"""$k $spaces= ${encodeString(v)}"""
        else s"""$k=${encodeString(v)}"""
      case Nested(k, v) =>
        val spaces = " " * math.max(keylen - k.length, 0)
        if spacy then s"""$k $spaces= ${convert(v, spacy, force, light)}"""
        else s"""$k=${convert(v, spacy, force, light)}"""
    }

    if !(spacy && attributes.raw.size > 1) then
      if light && attributes.legacyPositional.isEmpty then pairs.mkString("", "; ", "\n")
      else pairs.mkString(AttributesParser.open, "; ", AttributesParser.close)
    else if light && attributes.legacyPositional.isEmpty then pairs.mkString("", "\n", "\n")
    else pairs.mkString(s"${AttributesParser.open}\n\t", "\n\t", s"\n${AttributesParser.close}")
