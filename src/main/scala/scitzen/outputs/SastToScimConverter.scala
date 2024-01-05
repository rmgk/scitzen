package scitzen.outputs

import de.rmgk.Chain
import scitzen.bibliography.BibDB
import scitzen.parser.{AttributeDeparser, AttributesParser}
import scitzen.sast.*
import scitzen.sast.Attribute.{Named, Nested, Positional}
import scitzen.sast.DCommand.{BibQuery, Comment}

import scala.collection.immutable.ArraySeq

val astIncludesWhitespacelines = true

class SastToScimConverter(bibDB: BibDB):

  val attributeConverter = AttributesToScim(bibDB)

  def attributesToScim(
      attributes: Attributes,
      spacy: Boolean,
      force: Boolean,
      light: Boolean = false
  ): Chain[String] =
    val attrStr = AttributesToScim(bibDB).convert(attributes, spacy, force, light)
    if attrStr.isEmpty then Chain.nil
    else Chain(attrStr)

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
    sb.content match
      case Paragraph(content) =>
        val attrres = attributesToScim(sb.attributes, spacy = false, force = false)
        val res     = attrres :+ inlineToScim(content.inl)
        if astIncludesWhitespacelines
        then res
        else res :+ ""

      case Parsed(delimiter, blockContent) =>
        val content = toScimS(blockContent)
        delimiter.charAt(0) match
          case ':' =>
            Chain(
              "::" + BCommand.print(sb.command) +
              AttributesToScim(bibDB).convert(sb.attributes, force = false, spacy = false),
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
        val delimiter = "```"
        Chain(
          delimiter + BCommand.print(sb.command) + AttributesToScim(bibDB).convert(
            sb.attributes,
            spacy = false,
            force = false
          ),
          addIndent(text, "\t"),
          delimiter
        )

  def macroToScim(mcro: Directive, spacy: Boolean = false): String =
    mcro match
      case Directive(Comment, attributes) => s":%${attributes.target}"
      case Directive(BibQuery, _)         => macroToScim(bibDB.convert(mcro))
      case _ =>
        s":${DCommand.printMacroCommand(mcro.command)}${attributeConverter.convert(mcro.attributes, spacy, force = true)}"

  def inlineToScim(inners: Seq[Inline]): String =
    inners.map {
      case InlineText(str, 0) => str
      case InlineText(str, x) =>
        val qs = "\"" * x
        s":$qs[$str]$qs"
      case m: Directive => macroToScim(m)
    }.mkString("")

class AttributesToScim(bibDB: BibDB):

  def encodeText(text: Text): String =
    val value = SastToScimConverter(bibDB).inlineToScim(text.inl)
    AttributeDeparser.quote(
      forceEmpty = false,
      value,
      {
        case `text` => true
        case other  => false
      }
    )
  def encodeString(value: String): String =
    AttributeDeparser.quote(
      forceEmpty = true,
      value,
      {
        case Text(Nil) if value.isEmpty        => true
        case Text(Seq(InlineText(`value`, _))) => true
        case other                             => false
      }
    )

  def convert(attributesInput: Attributes, spacy: Boolean, force: Boolean, light: Boolean = false): String =
    val attributes = Attributes(attributesInput.raw.filter(p => !p.id.contains(' ')))
    if !force && attributes.raw.isEmpty then return ""
    val keylen = (attributes.raw.map { _.id.length }).maxOption.getOrElse(0)
    val pairs = attributes.raw.map {
      case Positional(v) => encodeText(v)
      case Named(k, v) =>
        val spaces = " " * math.max(keylen - k.length, 0)
        if spacy then s"""$k $spaces= ${encodeText(v)}"""
        else s"""$k=${encodeText(v)}"""
      case Nested(k, v) =>
        val spaces = " " * math.max(keylen - k.length, 0)
        if spacy then s"""$k $spaces= ${convert(v, spacy, force, light)}"""
        else s"""$k=${convert(v, spacy, force, light)}"""
    }

    val additionalNewline =
      if astIncludesWhitespacelines
      then ""
      else "\n"

    if !(spacy && attributes.raw.size > 1) then
      if light && attributes.positional.isEmpty then pairs.mkString("", "; ", additionalNewline)
      else pairs.mkString(AttributesParser.open, "; ", AttributesParser.close)
    else if light && attributes.positional.isEmpty then pairs.mkString("", "\n", additionalNewline)
    else pairs.mkString(s"${AttributesParser.open}\n\t", "\n\t", s"\n${AttributesParser.close}")
