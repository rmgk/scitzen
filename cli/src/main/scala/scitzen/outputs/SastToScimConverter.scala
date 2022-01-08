package scitzen.outputs

import cats.data.Chain
import cats.implicits.*
import fastparse.P
import scitzen.parser.AttributesParser
import scitzen.sast.*
import scitzen.sast.Attribute.{Nested, Plain, Positional}
import scitzen.sast.MacroCommand.Comment

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
      case Section(title, prefix, attributes) =>
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
          if stripped.isEmpty then tail else stripped :: tail
      )
        .reverse
    )

  def addIndent(lines: String, delimiter: String): String =
    lines.linesWithSeparators.map { line =>
      if line == "\n" then line
      else delimiter + line
    }.mkString

  def convertBlock(sb: Block): Chain[String] =
    val (remattr, command: String) = sb.attributes.raw.headOption match
      case Some(Positional(_, Some(value))) => (sb.attributes.copy(raw = sb.attributes.raw.drop(1)), value)
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
          delimiter + command + AttributesToScim.convert(remattr, spacy = false, force = false),
          addIndent(text, " ".repeat(delimiter.length)),
          delimiter
        )

  def macroToScim(mcro: Macro, spacy: Boolean = false): String =
    mcro match
      case Macro(Comment, attributes) => s":%${attributes.target}"
      case _ =>
        s":${MacroCommand.printMacroCommand(mcro.command)}${AttributesToScim.convert(mcro.attributes, spacy, force = true)}"

  def inlineToScim(inners: Seq[Inline]): String =
    inners.map {
      case InlineText(str) => str
      case m: Macro        => macroToScim(m)
    }.mkString("")

object AttributesToScim:
  val countQuotes: Regex = """(]"*)""".r

  def encodeText(text: Text): String =
    val value = SastToScimConverter.inlineToScim(text.inl)
    def parses(quoted: String): Boolean =
      val parsedAttr = fastparse.parse(quoted, AttributesParser.attribute(_))

      parsedAttr.get.value match {
        case Positional(`text`, _) => true
          //// format again to remove unimportant differences (mostly providence)
          //val transformedValue = SastToScimConverter.inlineToScim(t.inl)
          //value == transformedValue
        case other => false
      }

    def pickFirst(candidate: (() => String)*): Option[String] =
      candidate.view.map(_.apply()).find(parses)

    pickFirst(() => value, () => s""""$value"""", () => s"[$value]").getOrElse {
      val mi         = countQuotes.findAllIn(value)
      val quoteCount = if mi.isEmpty then 0 else mi.map(_.length).max
      val quotes     = "\"" * quoteCount
      s"""$quotes[$value]$quotes"""
    }

  def encodeString(value: String): String =
    def parses(quoted: String): Boolean =
      val parsedAttr = fastparse.parse(quoted, AttributesParser.stringValue(_))
      parsedAttr.get.value == value

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


