package scitzen.outputs

import cats.data.Chain
import cats.implicits._
import fastparse.P
import scitzen.generic.Sast
import scitzen.generic.Sast._
import scitzen.parser.MacroCommand.Comment
import scitzen.parser.{Attribute, Attributes, AttributesParser, Inline, InlineText, Macro, MacroCommand}

import scala.collection.immutable.ArraySeq
import scala.util.matching.Regex

case class SastToScimConverter() {

  def attributesToScim(
      attributes: Attributes,
      spacy: Boolean,
      force: Boolean,
      light: Boolean = false
  ): Chain[String] = {
    if (!force && attributes.raw.isEmpty) Chain.nil
    else Chain(AttributesToScim.convert(attributes, spacy, force, light))
  }

  def toScimS(b: Seq[Sast]): Chain[String] = {
    Chain.fromSeq(b).flatMap(toScim)
  }

  def toScim(sast: Sast): Chain[String] =
    sast match {
      case NoContent => Chain.empty

      case Section(title, prefix, attributes) =>
        (prefix + " " + inlineToScim(title.inline)) +:
          attributesToScim(attributes, spacy = true, force = false, light = true)

      case Slist(children) => Chain.fromSeq(children).flatMap {
          case SlistItem(marker, inner, NoContent) =>
            Chain(marker + inlineToScim(inner.inline))

          case SlistItem(marker, Text(inl), rest) =>
            (s"$marker" + inlineToScim(inl) + (if (rest.isInstanceOf[Slist]) "" else ":")) +: toScim(rest)
        }

      case SMacro(mcro) => Chain(macroToScim(mcro))

      case tlb: SBlock => convertBlock(tlb)
    }

  def stripLastEnd(strings: Chain[String]): Chain[String] =
    Chain.fromSeq(
      (strings.toList.reverse match {
        case Nil => Nil
        case head :: tail =>
          val stripped = head.stripTrailing()
          if (stripped.isEmpty) tail else stripped :: tail
      }).reverse
    )

  def addIndent(lines: String, delimiter: String): String =
    lines.linesWithSeparators.map { line =>
      if (line == "\n") line
      else delimiter + line
    }.mkString

  def convertBlock(sb: SBlock): Chain[String] = {
    val (remattr, command) = sb.attributes.raw.headOption match {
      case Some(Attribute("", command)) => (sb.attributes.copy(raw = sb.attributes.raw.drop(1)), command)
      case _                            => (sb.attributes, "")
    }
    sb.content match {

      case Paragraph(content) =>
        val attrres = attributesToScim(sb.attributes, spacy = false, force = false)
        attrres :+ inlineToScim(content.inline)

      case Parsed(delimiter, blockContent) =>
        val content = toScimS(blockContent)
        delimiter.charAt(0) match {
          case ':' =>
            ("::" + command +
              AttributesToScim.convert(remattr, force = false, spacy = false)) +:
              content.map(addIndent(_, "  ")) :+
              "::"
          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case ' ' | '\t' =>
            stripLastEnd(content.map(addIndent(_, delimiter)))
        }

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
    }
  }

  def macroToScim(mcro: Macro, spacy: Boolean = false): String = {
    mcro match {
      case Macro(Comment, attributes) => s":%${attributes.target}"
      case other =>
        s":${MacroCommand.print(mcro.command)}${AttributesToScim.convert(mcro.attributes, spacy, force = true)}"
    }
  }

  def inlineToScim(inners: Seq[Inline]): String =
    inners.map {
      case InlineText(str) => str
      case m: Macro        => macroToScim(m)
    }.mkString("")

}

object AttributesToScim {
  val countQuotes: Regex = """(]"*)""".r

  def encodeValue(value: String, isNamed: Boolean): String = {
    def parses(quoted: String): Boolean = {
      def parser[_: P]: P[Attribute] =
        if (isNamed) AttributesParser.positionalAttribute
        else AttributesParser.attribute

      val res = fastparse.parse(quoted, parser(_))
      res.get.value.value == value
    }

    def pickFirst(candidate: (() => String)*): Option[String] = {
      candidate.view.map(_.apply()).find(parses)
    }

    pickFirst(() => value, () => s""""$value"""", () => s"[$value]").getOrElse {
      val mi         = countQuotes.findAllIn(value)
      val quoteCount = if (mi.isEmpty) 0 else mi.map(_.length).max
      val quotes     = "\"" * quoteCount
      s"""$quotes[$value]$quotes"""
    }
  }

  def convert(attributes: Attributes, spacy: Boolean, force: Boolean, light: Boolean = false): String = {
    if (!force && attributes.raw.isEmpty) return ""
    val keylen = (0 +: attributes.raw.map(_.id.length)).max
    val pairs = attributes.raw.map {
      case Attribute("", v) => encodeValue(v, isNamed = false)
      case Attribute(k, v) =>
        val spaces = " " * math.max(keylen - k.length, 0)
        if (spacy) s"""$k $spaces= ${encodeValue(v, isNamed = true)}"""
        else s"""$k=${encodeValue(v, isNamed = true)}"""
    }

    if (!(spacy && attributes.raw.size > 1)) {
      if (light && attributes.positional.isEmpty) pairs.mkString("", "; ", "\n")
      else pairs.mkString(AttributesParser.open, "; ", AttributesParser.close)
    } else {
      if (light && attributes.positional.isEmpty) pairs.mkString("", "\n", "\n")
      else pairs.mkString(s"${AttributesParser.open}\n\t", "\n\t", s"\n${AttributesParser.close}")
    }
  }

}
