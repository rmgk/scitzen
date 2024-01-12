package scitzen.outputs

import de.rmgk.Chain
import scitzen.bibliography.BibDB
import scitzen.sast.Fusion.Atoms
import scitzen.parser.{AttributeDeparser, AttributesParser}
import scitzen.sast.*
import scitzen.sast.Attribute.{Named, Nested, Positional}
import scitzen.sast.DCommand.{BibQuery, Comment}

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.math.{max, min}

case class Indent(list: Int, listCheck: List[String], definition: Int, defCheck: List[String], delimited: Int):
  def formatLi: String = ("\t" * (delimited + definition + list))
  def format: String   = ("\t" * (delimited + definition + max(0, list - 1))) + (if list > 0 then "  " else "")

  @tailrec
  final def update(container: ::[Atom]): (String, Indent) =
    val head = container.head
    val check1 =
      head match
        case _: (ListAtom | Text | Directive) =>
          val other = listCheck.headOption.getOrElse("")
          if listCheck.nonEmpty
            && (head.isInstanceOf[ListAtom] && head.meta.indent.length <= other.length)
            || !head.meta.indent.startsWith(other)
          then
            return copy(list = max(0, list - 1), listCheck.drop(1)).update(container)
          else this
        case other => copy(list = 0, listCheck = Nil)
    val check2 =
      val other = check1.defCheck.headOption.getOrElse("")
      if !head.isInstanceOf[SpaceComment]
        && (check1.defCheck.nonEmpty
        && !head.meta.indent.startsWith(other))
      then return check1.copy(definition = max(0, check1.definition - 1), check1.defCheck.drop(1)).update(container)
      else check1

    container.head match
      case del: Delimiter =>
        val add  = if del.command == BCommand.Empty then -1 else 1
        val copy = check2.copy(delimited = max(0, check2.delimited + add))
        (if del.command == BCommand.Empty then copy.format else check2.format, copy)
      case ListAtom(marker, _, meta) =>
        (check2.formatLi, check2.copy(list = check2.list + 1, listCheck = meta.indent :: check2.listCheck))
      case _: DefinitionListAtom =>
        val nextIndent = container.tail.headOption.map(_.meta.indent).getOrElse("")
        val form       = check2.format
        (
          form,
          if nextIndent.length > form.length && nextIndent.startsWith(form)
          then check2.copy(definition = check2.definition + 1, defCheck = nextIndent :: check2.defCheck)
          else check2
        )
      case other =>
        (check2.format, check2)

class AtomToScimConverter(bibDB: BibDB):

  val attributeConverter = AttributesToScim(bibDB)

  def toScimS(atoms: Atoms, acc: Chain[String] = Chain.nil, indent: Indent = Indent(0, Nil, 0, Nil, 0)): Chain[String] =
    atoms match
      case Nil => acc
      case container :: rest =>
        val (format, indent2) = indent.update(::(container, rest))
        val chains = container match
          case Section(title, prefix, attributes, meta) =>
            Chain(format, prefix, " ") ++ inlineToScim(title.inl) ++ Chain("\n") ++ {
              val attrStr = attributeConverter.convert(attributes, spacy = true, force = false, light = true)
              if attrStr.isEmpty then Chain.nil
              else Chain(attrStr)
            }

          case TextAtom(Text(inl), meta) =>
            val actual = if container.meta.indent.startsWith(format) then container.meta.indent else format
            actual +: inlineToScim(inl) :+ "\n"

          case ListAtom(marker, content, meta) =>
            format +: marker +: inlineToScim(content) :+ "\n"

          case DefinitionListAtom(marker, content, meta) =>
            format +: marker +: inlineToScim(content) :+ "\n"

          case mcro: Directive => Chain(format, directive(mcro), "\n")

          case SpaceComment(text, meta) =>
            Chain(
              text.split("\\n", -1).map(_.stripTrailing()).mkString("\n")
            )

          case Delimiter(_, command, attributes, meta) =>
            Chain(
              format,
              "::",
              BCommand.print(command),
              AttributesToScim(bibDB).convert(attributes, force = false, spacy = false, light = false),
              "\n",
            )

          case Fenced(command, attributes, text, meta) =>
            val delimiter = "```"
            val indentS   = format
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
        toScimS(rest, acc ++ chains, indent2)

  def addIndent(lines: String, delimiter: String): String =
    lines.linesWithSeparators.map { line =>
      if line == "\n" then line
      else s"$delimiter$line"
    }.mkString

  def directive(dir: Directive, spacy: Boolean = false): String =
    dir match
      case Directive(Comment, attributes, meta) => s":%${attributes.target}"
      case Directive(BibQuery, _, meta)         => directive(bibDB.convert(dir))
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
        val spaces = " " * max(keylen - k.length, 0)
        if spacy then s"""$k $spaces= ${encodeText(v)}"""
        else s"""$k=${encodeText(v)}"""
      case Nested(k, v) =>
        val spaces = " " * max(keylen - k.length, 0)
        if spacy then s"""$k $spaces= ${convert(v, spacy, force, light)}"""
        else s"""$k=${convert(v, spacy, force, light)}"""
    }

    if !(spacy && attributes.raw.size > 1) then
      if light && attributes.positional.isEmpty then pairs.mkString("; ")
      else pairs.mkString(AttributesParser.open, "; ", AttributesParser.close)
    else if light && attributes.positional.isEmpty then pairs.mkString("\n")
    else pairs.mkString(s"${AttributesParser.open}\n\t", "\n\t", s"\n${AttributesParser.close}")
