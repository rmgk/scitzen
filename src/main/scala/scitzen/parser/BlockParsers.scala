package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.parser.sast.{Attributes, Sast}
import scitzen.parser.sast.{Block, Paragraph, Section, SpaceComment, Text}

object BlockParsers {

  val paragraphInlines = InlineParsers(
    "\n",
    { tp =>
      implicit val p: P[_] = tp
      eol ~ spaceLine
    }
  )

  val sectionInlines = InlineParsers("\n", eol(_))

  def paragraph[_: P]: P[Block] =
    P((
      (AttributesParser.braces ~ spaceLine).? ~
        withProv(paragraphInlines.full)
    ).map {
      case (attrOpt, ((inlines, end), prov)) =>
        //val endline = if (end.contains('\n')) inlines :+ InlineText("\n") else inlines
        Block(Attributes(attrOpt.getOrElse(Nil), prov), Paragraph(Text(inlines)))
    })

  def sectionStart[_: P]: P[String] = P(CharsWhileIn("=#").! ~ " ")
  def sectionTitle[_: P]: P[Section] =
    P(sectionStart
      ~ withProv(sectionInlines.full)
      ~ (AttributesParser.braces ~ spaceLine | AttributesParser.noBraces).?)
      .map {
        case (prefix, ((inlines, elo), prov), attrl) =>
          //val inlines = Parse.inlineUnwrap(inl, prov)
          Section(Text(inlines), prefix, scitzen.parser.sast.Attributes(attrl.getOrElse(Nil), prov))
      }

  def extendedWhitespace[_: P]: P[Block] =
    P(withProv(
      (significantSpaceLine.rep(1) |
        MacroParsers.comment.rep(1)).rep(1).!
    )).map {
      case (str, prov) =>
        Block(scitzen.parser.sast.Attributes(Nil, prov), SpaceComment(str))
    }

  def alternatives[_: P]: P[Sast] =
    P(extendedWhitespace |
      ListParsers.list |
      DelimitedBlockParsers.anyDelimited |
      sectionTitle |
      MacroParsers.full ~ spaceLine |
      paragraph)

}
