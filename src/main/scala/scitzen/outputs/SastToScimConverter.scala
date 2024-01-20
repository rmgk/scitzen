package scitzen.outputs

import de.rmgk.Chain
import scitzen.bibliography.BibDB
import scitzen.sast.Fusion.Atoms
import scitzen.parser.{AttributeDeparser, AttributesParser}
import scitzen.sast.*
import scitzen.sast.DCommand.{BibQuery, Comment}

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.math.{max, min}

class SastToScimConverter(bibDB: BibDB):

  val attributeConverter = AttributesToScim(bibDB)

  def convertSequence(
      atoms: Seq[Sast],
      indent: String = ""
  ): Chain[String] =
    atoms.to(Chain).flatMap(sast => convertSast(sast, indent))

  private def convertSast(container: Sast, indent: String) = {
    container match
      case atom: Atom       => convertAtom(atom, indent)
      case FusedList(items) => convertFusedList(items, indent)
      case Paragraph(content) =>
        content.to(Chain).flatMap(at => convertAtom(at, indent))
      case FusedDelimited(del, cont) =>
        convertAtom(del, indent) ++ convertSequence(cont.toList, s"$indent\t") ++ Chain(
          indent,
          del.marker,
          "\n"
        )
      case FusedDefinitions(children) =>
        children.to(Chain).flatMap: fdi =>
          convertAtom(fdi.head, indent) ++ convertSequence(fdi.content, s"$indent\t")
  }
  def convertFusedList(items: Seq[FusedListItem], indent: String): Chain[String] =
    items.to(Chain).flatMap: item =>
      convertAtom(item.head, indent) ++ item.rest.flatMap(r => convertAtom(r, s"$indent  ")) ++
      convertFusedList(item.children, s"$indent\t")

  def convertAtom(atom: Atom, indent: String): Chain[String] = {
    atom match
      case Section(title, prefix, attributes, meta) =>
        Chain(indent, prefix, " ") ++ inlineToScim(title.inl) ++ Chain("\n") ++ {
          val attrStr = attributeConverter.convert(attributes, spacy = true, force = false, light = true)
          if attrStr.isEmpty then Chain.nil
          else Chain(attrStr)
        }

      case TextAtom(inl, meta) =>
        val actual = if meta.indent.startsWith(indent) then meta.indent else indent
        actual +: inlineToScim(inl) :+ "\n"

      case ListAtom(marker, content, meta) =>
        indent +: marker +: inlineToScim(content) :+ "\n"

      case DefinitionListAtom(marker, content, meta) =>
        indent +: marker +: inlineToScim(content) :+ "\n"

      case mcro: Directive =>
        val actual = if mcro.meta.indent.startsWith(indent) then mcro.meta.indent else indent
        Chain(actual, directive(mcro), "\n")

      case SpaceComment(text, meta) =>
        Chain(
          text.split("\\n", -1).map(_.stripTrailing()).mkString("\n")
        )

      case Delimiter(_, command, attributes, meta) =>
        Chain(
          indent,
          "::",
          BCommand.print(command),
          AttributesToScim(bibDB).convert(attributes, force = false, spacy = false, light = false),
          "\n",
        )

      case Fenced(command, attributes, text, meta) =>
        val delimiter = "```"
        val indentS   = indent
        Chain(
          indentS,
          delimiter,
          BCommand.print(command),
          AttributesToScim(bibDB).convert(
            attributes,
            spacy = false,
            force = false
          ),
          "\n",
          addIndent(text, s"${indentS}\t"),
          "\n",
          indentS,
          delimiter,
          "\n"
        )
  }

  def addIndent(lines: String, delimiter: String): String =
    lines.linesWithSeparators.map { line =>
      if line == "\n" then line
      else s"$delimiter$line"
    }.mkString

  def directive(dir: Directive, spacy: Boolean = false): String =
    dir match
      case Directive(Comment, attributes, meta) => s":%${attributes.target}"
      case Directive(BibQuery, _, meta)         => directive(bibDB.convertBibQuery(dir))
      case _ =>
        s":${DCommand.printMacroCommand(dir.command)}${attributeConverter.convert(dir.attributes, spacy, force = true)}"

  def inlineToScim(inners: Seq[Inline]): Chain[String] =
    inners.iterator.map {
      case InlineText(str, 0) => str
      case InlineText(str, x) =>
        val qs = "\"" * x
        s":$qs[$str]$qs"
      case m: Directive => directive(m)
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

  def convert(attributesInput: Attributes, spacy: Boolean, force: Boolean, light: Boolean = false): String =
    val attributes = Attributes(attributesInput.raw.filter(p => !p.id.contains(' ')))
    if !force && attributes.raw.isEmpty then return ""
    val keylen = (attributes.raw.map { _.id.length }).maxOption.getOrElse(0)
    val pairs = attributes.raw.map {
      case Attribute("", _, v) => encodeText(v)
      case Attribute(k,_, v) =>
        val spaces = " " * max(keylen - k.length, 0)
        if spacy then s"""$k $spaces= ${encodeText(v)}"""
        else s"""$k=${encodeText(v)}"""
//      case Nested(k, v) =>
//        val spaces = " " * max(keylen - k.length, 0)
//        if spacy then s"""$k $spaces= ${convert(v, spacy, force, light)}"""
//        else s"""$k=${convert(v, spacy, force, light)}"""
    }

    if !(spacy && attributes.raw.size > 1) then
      if light && attributes.positional.isEmpty then pairs.mkString("; ")
      else pairs.mkString(AttributesParser.open, "; ", AttributesParser.close)
    else if light && attributes.positional.isEmpty then pairs.mkString("\n")
    else pairs.mkString(s"${AttributesParser.open}\n\t", "\n\t", s"\n${AttributesParser.close}")
