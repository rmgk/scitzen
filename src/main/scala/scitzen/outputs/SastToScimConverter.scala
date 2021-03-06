package scitzen.outputs

import cats.data.Chain
import cats.implicits._
import fastparse.P
import scitzen.parser.AttributesParser
import scitzen.sast.MacroCommand.Comment
import scitzen.sast._

import scala.collection.immutable.ArraySeq
import scala.util.matching.Regex

object SastToScimConverter:

  def attributesToScim(
      attributes: Attributes,
      spacy: Boolean,
      force: Boolean,
      light: Boolean = false
  ): Chain[String] =
    if !force && attributes.raw.isEmpty then Chain.nil
    else Chain(AttributesToScim.convert(attributes, spacy, force, light))

  def toScimS(b: Seq[Sast]): Chain[String] =
    Chain.fromSeq(b).flatMap(toScim)

  def toScim(sast: Sast): Chain[String] =
    sast match
      case Section(title, prefix, attributes, prov) =>
        (prefix + " " + inlineToScim(title.inl)) +:
          attributesToScim(attributes, spacy = true, force = false, light = true)

      case Slist(children) => Chain.fromSeq(children).flatMap {
          case ListItem(marker, inner, None) =>
            Chain(marker + inlineToScim(inner.inl))

          case ListItem(marker, Text(inl), Some(rest)) =>
            Chain(
              s"$marker" + inlineToScim(inl) + (if rest.isInstanceOf[Slist] then "" else ":"),
              toScim(rest).iterator.mkString("\n").stripTrailing
            )
        }

      case mcro: Macro => Chain(macroToScim(mcro))

      case tlb: Block => convertBlock(tlb)

  def stripLastEnd(strings: Chain[String]): Chain[String] =
    Chain.fromSeq(
      (strings.toList.reverse match
        case Nil => Nil
        case head :: tail =>
          val stripped = head.stripTrailing()
          if stripped.isEmpty then tail else stripped :: tail)
        .reverse
    )

  def addIndent(lines: String, delimiter: String): String =
    lines.linesWithSeparators.map { line =>
      if line == "\n" then line
      else delimiter + line
    }.mkString

  def convertBlock(sb: Block): Chain[String] =
    val (remattr, command) = sb.attributes.raw.headOption match
      case Some(Attribute("", command)) => (sb.attributes.copy(raw = sb.attributes.raw.drop(1)), command)
      case _                            => (sb.attributes, Text(Nil))
    sb.content match

      case Paragraph(content) =>
        val attrres = attributesToScim(sb.attributes, spacy = false, force = false)
        attrres :+ inlineToScim(content.inl) :+ ""

      case Parsed(delimiter, blockContent) =>
        val content = toScimS(blockContent)
        delimiter.charAt(0) match
          case ':' =>
            Chain(
              "::" + command.str +
                AttributesToScim.convert(remattr, force = false, spacy = false),
              content.map(addIndent(_, "  ")).iterator.mkString("\n").stripTrailing(),
              "::"
            )
          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case ' ' | '\t' =>
            stripLastEnd(content.map(addIndent(_, delimiter)))

      case SpaceComment(text) =>
        Chain.fromSeq(ArraySeq.unsafeWrapArray(
          text.stripLineEnd.split("\\n", -1).map(_.trim)
        ))
      case Fenced(text) =>
        val delimiter = "``"
        Chain(
          delimiter + command.str + AttributesToScim.convert(remattr, spacy = false, force = false),
          addIndent(text, " ".repeat(delimiter.length)),
          delimiter
        )

  def macroToScim(mcro: Macro, spacy: Boolean = false): String =
    mcro match
      case Macro(Comment, attributes, _) => s":%${attributes.target}"
      case _ =>
        s":${MacroCommand.printMacroCommand(mcro.command)}${AttributesToScim.convert(mcro.attributes, spacy, force = true)}"

  def inlineToScim(inners: Seq[Inline]): String =
    inners.map {
      case InlineText(str) => str
      case m: Macro        => macroToScim(m)
    }.mkString("")

object AttributesToScim:
  val countQuotes: Regex = """(]"*)""".r

  def encodeValue(text: Text, isNamed: Boolean): String =
    val value = SastToScimConverter.inlineToScim(text.inl)
    def parses(quoted: String): Boolean =
      def parser(implicit p: P[?]): P[Attribute] =
        if isNamed then AttributesParser.positionalAttribute
        else AttributesParser.attribute

      val parsedAttr  = fastparse.parse(quoted, parser(_))
      val reformatted = SastToScimConverter.inlineToScim(parsedAttr.get.value.text.inl)
      reformatted == value

    def pickFirst(candidate: (() => String)*): Option[String] =
      candidate.view.map(_.apply()).find(parses)

    pickFirst(() => value, () => s""""$value"""", () => s"[$value]").getOrElse {
      val mi         = countQuotes.findAllIn(value)
      val quoteCount = if mi.isEmpty then 0 else mi.map(_.length).max
      val quotes     = "\"" * quoteCount
      s"""$quotes[$value]$quotes"""
    }

  def convert(attributes: Attributes, spacy: Boolean, force: Boolean, light: Boolean = false): String =
    if !force && attributes.raw.isEmpty then return ""
    val keylen = (0 +: attributes.raw.map(_.id.length)).max
    val pairs = attributes.raw.map {
      case Attribute("", v) => encodeValue(v, isNamed = false)
      case Attribute(k, v) =>
        val spaces = " " * math.max(keylen - k.length, 0)
        if spacy then s"""$k $spaces= ${encodeValue(v, isNamed = true)}"""
        else s"""$k=${encodeValue(v, isNamed = true)}"""
    }

    if !(spacy && attributes.raw.size > 1) then
      if light && attributes.positional.isEmpty then pairs.mkString("", "; ", "\n")
      else pairs.mkString(AttributesParser.open, "; ", AttributesParser.close)
    else if light && attributes.positional.isEmpty then pairs.mkString("", "\n", "\n")
    else pairs.mkString(s"${AttributesParser.open}\n\t", "\n\t", s"\n${AttributesParser.close}")
