package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._
import scitzen.sast.{Attributes, Block, Paragraph, Sast, Section, SpaceComment, Text}

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
      case (attrOpt, ((inlines, _), prov)) =>
        //val endline = if (end.contains('\n')) inlines :+ InlineText("\n") else inlines
        Block(scitzen.sast.Attributes(attrOpt.getOrElse(Nil)), Paragraph(Text(inlines)), prov)
    })

  def sectionStart[_: P]: P[String] = P(CharsWhileIn("=#").! ~ " ")
  def sectionTitle[_: P]: P[Section] =
    P(sectionStart
      ~ withProv(sectionInlines.full)
      ~ (AttributesParser.braces ~ spaceLine | AttributesParser.noBraces).?)
      .map {
        case (prefix, ((inlines, _), prov), attrl) =>
          //val inlines = Parse.inlineUnwrap(inl, prov)
          Section(scitzen.sast.Text(inlines), prefix, Attributes(attrl.getOrElse(Nil)), prov)
      }

  def extendedWhitespace[_: P]: P[Block] =
    P(withProv(
      (significantSpaceLine.rep(1) |
        MacroParsers.comment.rep(1)).rep(1).!
    )).map {
      case (str, prov) =>
        Block(scitzen.sast.Attributes(Nil), SpaceComment(str), prov)
    }

  def alternatives[_: P]: P[Sast] =
    P(extendedWhitespace |
      ListParsers.list |
      DelimitedBlockParsers.anyDelimited |
      sectionTitle |
      MacroParsers.full ~ spaceLine |
      paragraph)

}
