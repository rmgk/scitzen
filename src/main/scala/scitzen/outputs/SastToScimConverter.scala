package scitzen.outputs

import cats.data.Chain
import cats.implicits._
import scitzen.generic.Sast
import scitzen.generic.Sast._
import scitzen.parser.MacroCommand.{Comment, Def, Other}
import scitzen.parser.{Attribute, AttributesParser, Inline, InlineText, Macro, MacroCommand}

import scala.collection.compat.immutable
import scala.util.matching.Regex


case class SastToScimConverter() {

  //val attributeEscapes = """[;=\]}\n]|^[\s"\[]|\s$""".r
  val countQuotes = """(]"*)""".r

  def encodeValue(value: String, isNamed: Boolean): String = {
    def parses(quoted: String) = {
      val res = fastparse.parse(quoted,
                                if (isNamed)
                                  AttributesParser.positionalAttribute(_)
                                else AttributesParser.attribute(_))
      res.get.value.value == value
    }

    def pickFirst(candidate: (() => String)*): Option[String] = {
      candidate.view.map(_.apply()).find(parses)
    }


    pickFirst(() => value,
              () => s""""$value"""",
              () => s"[$value]").getOrElse {
      val mi         = countQuotes.findAllIn(value)
      val quoteCount = if (mi.isEmpty) 0 else mi.map(_.length).max
      val quotes     = "\"" * quoteCount
      s"""$quotes[$value]$quotes"""
    }
  }


  def attributesToScim(attributes: Seq[Attribute], spacy: Boolean, force: Boolean, light: Boolean = false): Chain[String] = {
    if (!force && attributes.isEmpty) Chain.nil
    else Chain(attributesToScimF(attributes, spacy, force, light))
  }


  def attributesToScimF(attributes: Seq[Attribute], spacy: Boolean, force: Boolean, light: Boolean = false): String = {
    if (!force && attributes.isEmpty) return ""
    val keylen = (0 +: attributes.map(_.id.length)).max
    val pairs  = attributes.map {
      case Attribute("", v) => encodeValue(v, isNamed = false)
      case Attribute(k, v)  =>
        val spaces = " " * math.max(keylen - k.length, 0)
        if (spacy) s"""$k $spaces= ${encodeValue(v, isNamed = true)}"""
        else s"""$k=${encodeValue(v, isNamed = true)}"""
    }

    if (!(spacy && attributes.size > 1)) {
      if (light) pairs.mkString("", "; ", "\n")
      else pairs.mkString(AttributesParser.open, "; ", AttributesParser.close)
    } else {
      if (light) pairs.mkString("", "\n", "\n")
      else pairs.mkString(s"${AttributesParser.open}\n\t", "\n\t", s"\n${AttributesParser.close}")
    }
  }

  def toScimS(b: Seq[Sast]): Chain[String] = {
    Chain.fromSeq(b).flatMap(toScim(_))
  }

  def toScim(sast: Sast): Chain[String] = sast match {
    case NoContent => Chain.empty

    case Section(title, level, attributes) =>
      ("=" * level + " " + inlineToScim(title.inline)) +:
      attributesToScim(attributes.raw, spacy = true, force = false, light = true)

    case Slist(children) => Chain.fromSeq(children).flatMap {
      case SlistItem(marker, inner, NoContent) =>
        Chain(marker + inlineToScim(inner.inline))

      case SlistItem(marker, Text(inl), rest) =>
        (s"$marker" + inlineToScim(inl) + (if (rest.isInstanceOf[Slist]) "" else ":")) +: toScim(rest)
    }

    case SMacro(m @ Macro(Def, attributes)) => Chain(macroToScimRaw(m, spacy = true))
    case SMacro(mcro)                       => Chain(macroToScim(mcro))

    case tlb: SBlock => convertBlock(tlb)
  }

  private val fencedRegex: Regex = "`{3,}".r

  def stripLastEnd(strings: Chain[String]): Chain[String] = Chain.fromSeq(
    (strings.toList.reverse match {
      case Nil          => Nil
      case head :: tail =>
        val stripped = head.stripTrailing()
        if (stripped.isEmpty) tail else stripped :: tail
    }).reverse)

  def convertBlock(sb: SBlock): Chain[String] = sb.content match {

    case Paragraph(content) =>
      Chain(inlineToScim(content.inline))

    case Parsed(delimiter, blockContent) =>
      val strippedContent = toScimS(blockContent)
      delimiter.charAt(0) match {
        case '=' =>
          (delimiter +
           attributesToScimF(sb.attributes.raw, force = false, spacy = false)) +:
          strippedContent :+
          delimiter
        // space indented blocks are currently only used for description lists
        // they are parsed and inserted as if the indentation was not present
        case ' ' | '\t' =>
          val indented = strippedContent.map(_.linesWithSeparators.map(delimiter + _).mkString)
          stripLastEnd(indented)
      }

    case SpaceComment(text) =>
      Chain.fromSeq(immutable.ArraySeq.unsafeWrapArray(
        text.stripLineEnd.split("\\n", -1).map(_.trim)))
    case Fenced(text)       =>
      val foundlen  = fencedRegex.findAllMatchIn(text).map(r => r.end - r.start).maxOption.getOrElse(0)
      val delimiter = "`" * math.max(3, foundlen)
      Chain(delimiter + attributesToScimF(sb.attributes.raw, spacy = false, force = false),
            text,
            delimiter)
  }


  def macroToScimRaw(mcro: Macro, spacy: Boolean = false): String = {
    s":${MacroCommand.print(mcro.command)}${attributesToScimF(mcro.attributes.all, spacy, force = true)}"
  }

  def inlineToScim(inners: Seq[Inline]): String = inners.map {
    case InlineText(str) => str
    case m: Macro        => macroToScim(m)
  }.mkString("")

  def macroToScim(m: Macro): String = m match {
    case Macro(Other("horizontal-rule"), attributes) => attributes.target
    case Macro(Comment, attributes)                  => s":%${attributes.target}"
    case other                                       => macroToScimRaw(other)
  }
}
