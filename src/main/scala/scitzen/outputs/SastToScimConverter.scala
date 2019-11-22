package scitzen.outputs

import scitzen.generic.Sast
import scitzen.generic.Sast._
import scitzen.parser.MacroCommand.{Comment, Def, Other, Quote}
import scitzen.parser.{Attribute, Inline, InlineText, Macro, MacroCommand}
import cats.data.Chain
import cats.implicits._


case class SastToScimConverter() {

  val attributeEscapes = """[;=\]\n]|^[\s"\[]|\s$""".r
  val countQuotes = """(]"*)""".r

  def encodeValue(v: String): String = {
    if (!attributeEscapes.pattern.matcher(v).find()) v
    else if (!v.contains("\"")) s""""$v""""
    else {
      val mi         = countQuotes.findAllIn(v)
      val quoteCount = if (mi.isEmpty) 0 else mi.map(_.length).max
      val quotes     = "\"" * quoteCount
      s"""$quotes[$v]$quotes"""
    }
  }

  def attributesToScim(attributes: Seq[Attribute], spacy: Boolean, force: Boolean, light: Boolean = false): Chain[String] = {
        if (!force && attributes.isEmpty) Chain.nil
    else Chain(attributesToScimF(attributes, spacy, force, light))
  }


  def attributesToScimF(attributes: Seq[Attribute], spacy: Boolean, force: Boolean, light: Boolean = false): String = {
    if (!force && attributes.isEmpty) return ""
    val keylen = (0 +: attributes.map(_.id.length)).max
    val pairs = attributes.map {
      case Attribute("", v) => encodeValue(v)
      case Attribute(k, v)  =>
        val spaces = " " * math.max(keylen - k .length, 0)
        if (spacy) s"""$k $spaces= ${encodeValue(v)}"""
        else s"""$k=${encodeValue(v)}"""
    }

    if (!(spacy && attributes.size > 1)) {
      if (light) pairs.mkString("", "; ", "\n")
      else pairs.mkString("[", "; ", "]")
    } else {
      if (light) pairs.mkString("", "\n", "\n")
      else pairs.mkString("[\n\t", "\n\t", "\n]")
    }
  }

  def toScimS(b: Seq[Sast])(implicit nestingLevel: Scope = new Scope(1)): Chain[String] = {
    Chain.fromSeq(b).flatMap(toScim(_))
  }

  def toScim(sast: Sast)(implicit nestingLevel: Scope = new Scope(1)): Chain[String] = sast match {
    case Section(title, sc, attributes) =>
      ("=" * nestingLevel.level + " " + inlineToScim(title.inline)) +:
      (attributesToScim(attributes.raw, spacy = true, force = false, light = true) ++
       toScimS(sc)(nestingLevel.inc))

    case Slist(children) => Chain.fromSeq(children).flatMap {
      case SlistItem(marker, TLBlock(_, Paragraph(Text(inl))) :: rest) =>
        (s"$marker" + inlineToScim(inl)) +: toScimS(rest)
      case SlistItem(marker, inner)                                    =>
        marker +: toScimS(inner)
    }

    case MacroBlock(m @ Macro(Def, attributes)) => Chain(macroToScimRaw(m, spacy = true))
    case MacroBlock(mcro)                       => Chain(macroToScim(mcro))

    case tlb: TLBlock => tlBlockToScim(tlb)
  }

  def tlBlockToScim(sb: TLBlock)(implicit nestingLevel: Scope = new Scope(1)): Chain[String] = sb.content match {

      case Paragraph(content) =>
        //attributesToScim(sb.attr.raw, spacy = false, force = false) ++
        Chain(inlineToScim(content.inline))

      case ParsedBlock(delimiter, blockContent) =>
        delimiter.charAt(0) match {
          case '=' => (delimiter + attributesToScimF(sb.attr.raw, force = false, spacy = false)) +: toScimS(blockContent) :+ delimiter
          // space indented blocks are currently only used for description lists
          // they are parsed and inserted as if the indentation was not present
          case ' ' | '\t' =>
            attributesToScim(sb.attr.raw, spacy = false, force = false) ++ Chain.fromSeq(
              toScimS(blockContent)
              .iterator
              .flatMap(line => line.split("\n", -1))
              .map {
                case line if line.forall(_.isWhitespace) => ""
                case line                                => s"$delimiter$line"
              }.toList)
        }

      case RawBlock("comment|space", text) =>
        Chain.fromSeq(text.stripLineEnd.split("\\n", -1).map(_.trim))
      case RawBlock(delimiter, text) =>
        Chain(delimiter + attributesToScimF(sb.attr.raw, spacy = false, force = false),
              text,
              delimiter)
  }


  def macroToScimRaw(mcro: Macro, spacy: Boolean = false): String = {
    s":${MacroCommand.print(mcro.command)}${attributesToScimF(mcro.attributes.all, spacy, force = true)}"
  }

  def inlineToScim(inners: Seq[Inline]): String = inners.map {
    case InlineText(str)         => str
    case m: Macro                => macroToScim(m)
  }.mkString("")

  def macroToScim(m: Macro): String = m match {
    case Macro(Other("horizontal-rule"), attributes) => attributes.target
    case Macro(Comment, attributes) => s":%${attributes.target}\n"
    case Macro(Quote(q), inner2) => s":$q${inner2.target}$q"
    case other => macroToScimRaw(other)
  }
}
