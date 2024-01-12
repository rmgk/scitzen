package scitzen.outputs

import de.rmgk.Chain
import scitzen.bibliography.BibDB
import scitzen.parser.Atoms.{Atom, Container, DefinitionListAtom, Delimiter, ListAtom}
import scitzen.parser.Fusion.Atoms
import scitzen.parser.{AttributeDeparser, AttributesParser}
import scitzen.sast.*
import scitzen.sast.Attribute.{Named, Nested, Positional}
import scitzen.sast.DCommand.{BibQuery, Comment}

import scala.collection.immutable.ArraySeq

class AtomToScimConverter(bibDB: BibDB):

  val attributeConverter = AttributesToScim(bibDB)

  def toScimS(b: Atoms): Chain[String] =
    Chain.from(b).flatMap(toScim)

  def toScim(container: Container[Atom]): Chain[String] =
    container.content match
      case Section(title, prefix, attributes) =>
        Chain(container.indent, prefix, " ") ++ inlineToScim(title.inl) ++ Chain("\n") ++ {
          val attrStr = attributeConverter.convert(attributes, spacy = true, force = false, light = true)
          if attrStr.isEmpty then Chain.nil
          else Chain(attrStr)
        }

      case Text(inl) =>
        container.indent +: inlineToScim(inl)

      case ListAtom(marker, content) =>
        container.indent +: marker +: inlineToScim(content)

      case DefinitionListAtom(marker, content) =>
        container.indent +: marker +: inlineToScim(content)

      case mcro: Directive => Chain(directive(mcro), "\n")

      case SpaceComment(text) =>
        Chain(
          text.split("\\n", -1).map(_.stripTrailing()).mkString("\n")
        )

      case Delimiter(_, command, attributes) =>
        Chain(
          container.indent,
          "::",
          BCommand.print(command),
          AttributesToScim(bibDB).convert(attributes, force = false, spacy = false, light = false),
          "\n",
        )

      case Fenced(command, attributes, text, _, _) =>
        val delimiter = "```"
        Chain(
          container.indent,
          delimiter,
          BCommand.print(command),
          AttributesToScim(bibDB).convert(
            attributes,
            spacy = false,
            force = false
          ),
          "\n",
          addIndent(text, s"${container.indent}\t"),
          "\n",
          container.indent,
          delimiter,
          "\n"
        )

  def addIndent(lines: String, delimiter: String): String =
    lines.linesWithSeparators.map { line =>
      if line == "\n" then line
      else s"$delimiter$line"
    }.mkString

  def directive(dir: Directive, spacy: Boolean = false): String =
    dir match
      case Directive(Comment, attributes) => s":%${attributes.target}"
      case Directive(BibQuery, _)         => directive(bibDB.convert(dir))
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
    val value = AtomToScimConverter(bibDB).inlineToScim(text.inl).mkString("")
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
