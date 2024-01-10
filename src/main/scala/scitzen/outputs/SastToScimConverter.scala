package scitzen.outputs

import de.rmgk.Chain
import scitzen.bibliography.BibDB
import scitzen.parser.{AttributeDeparser, AttributesParser}
import scitzen.sast.*
import scitzen.sast.Attribute.{Named, Nested, Positional}
import scitzen.sast.DCommand.{BibQuery, Comment}

import scala.collection.immutable.ArraySeq

class SastToScimConverter(bibDB: BibDB):

  val attributeConverter = AttributesToScim(bibDB)

  def attributesToScim(
      attributes: Attributes,
      spacy: Boolean,
      force: Boolean,
      light: Boolean = false
  ): Chain[String] =
    val attrStr = attributeConverter.convert(attributes, spacy, force, light)
    if attrStr.isEmpty then Chain.nil
    else Chain(attrStr)

  def toScimS(b: Seq[Sast]): Chain[String] =
    Chain.from(b).flatMap(toScim)

  def toScim(sast: Sast): Chain[String] =
    sast match
      case Section(title, prefix, attributes) =>
        Chain(prefix, " ") ++ inlineToScim(title.inl) ++ ("\n" +:
        attributesToScim(attributes, spacy = true, force = false, light = true))

      case Slist(children) => Chain.from(children).flatMap {
          case ListItem(marker, inner, None) =>
            marker +: inlineToScim(inner.inl) :+ "\n"

          case ListItem(marker, Text(inl), Some(rest)) =>
            Chain(
              Chain(marker),
              inlineToScim(inl),
              Chain("\n"),
              toScim(rest)
            ).flatten
        }

      case mcro: Directive => Chain(macroToScim(mcro), "\n")

      case SpaceComment(text) =>
        Chain(
          text.split("\\n", -1).map(_.stripTrailing()).mkString("\n")
        )

      case tlb: Block => convertBlock(tlb)

  def addIndent(lines: String, delimiter: String): String =
    lines.linesWithSeparators.map { line =>
      if line == "\n" then line
      else delimiter + line
    }.mkString

  def convertBlock(sb: Block): Chain[String] =
    sb.content match
      case Paragraph(content) =>
        val attrres = attributesToScim(sb.attributes, spacy = false, force = false)
        val inner   = inlineToScim(content.inl)
        if attrres.nonEmpty
        then Chain(attrres, Chain("\n"), inner).flatten
        else inner

      case Parsed(delimiter, blockContent) =>
        val content = toScimS(blockContent).mkString
        delimiter.charAt(0) match
          case ':' =>
            Chain(
              "::",
              BCommand.print(sb.command),
              AttributesToScim(bibDB).convert(sb.attributes, force = false, spacy = false),
              "\n",
              addIndent(content, "\t"),
              "::\n"
            )
          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case ' ' | '\t' =>
            Chain(addIndent(content, delimiter))

      case Fenced(text) =>
        val delimiter = "```"
        Chain(
          delimiter,
          BCommand.print(sb.command),
          AttributesToScim(bibDB).convert(
            sb.attributes,
            spacy = false,
            force = false
          ),
          "\n",
          addIndent(text, "\t"),
          "\n",
          delimiter,
          "\n"
        )

  def macroToScim(mcro: Directive, spacy: Boolean = false): String =
    mcro match
      case Directive(Comment, attributes) => s":%${attributes.target}"
      case Directive(BibQuery, _)         => macroToScim(bibDB.convert(mcro))
      case _ =>
        s":${DCommand.printMacroCommand(mcro.command)}${attributeConverter.convert(mcro.attributes, spacy, force = true)}"

  def inlineToScim(inners: Seq[Inline]): Chain[String] =
    inners.iterator.map {
      case InlineText(str, 0) => str
      case InlineText(str, x) =>
        val qs = "\"" * x
        s":$qs[$str]$qs"
      case m: Directive => macroToScim(m)
    }.to(Chain)

class AttributesToScim(bibDB: BibDB):

  def encodeText(text: Text): String =
    val value = SastToScimConverter(bibDB).inlineToScim(text.inl).mkString("")
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

    if !(spacy && attributes.raw.size > 1) then
      if light && attributes.positional.isEmpty then pairs.mkString("; ")
      else pairs.mkString(AttributesParser.open, "; ", AttributesParser.close)
    else if light && attributes.positional.isEmpty then pairs.mkString("\n")
    else pairs.mkString(s"${AttributesParser.open}\n\t", "\n\t", s"\n${AttributesParser.close}")
