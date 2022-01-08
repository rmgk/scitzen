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

  def paragraph[_p: P]: P[Block] =
    P((
      (AttributesParser.braces ~ spaceLine).? ~
        withProv(paragraphInlines.full)
    ).map {
      case (attrOpt, ((inlines, _), prov)) =>
        //val endline = if (end.contains('\n')) inlines :+ InlineText("\n") else inlines
        Block(scitzen.sast.Attributes(attrOpt.getOrElse(Nil)), Paragraph(Text(inlines)), prov)
    })

  def sectionStart[_p: P]: P[String] = P(CharsWhileIn("=#").! ~ " ")
  def sectionTitle[_p: P]: P[Section] =
    P(sectionStart
      ~ withProv(sectionInlines.full)
      ~/ (AttributesParser.braces ~ spaceLine | AttributesParser.noBraces).?)
      .map {
        case (prefix, ((inlines, _), prov), attrl) =>
          //val inlines = Parse.inlineUnwrap(inl, prov)
          Section(scitzen.sast.Text(inlines), prefix, Attributes(attrl.getOrElse(Nil)))(prov)
      }

  def extendedWhitespace[_p: P]: P[Block] =
    P(withProv(
      (significantSpaceLine.rep(1) |
        MacroParsers.comment.rep(1)).rep(1).!
    )).map {
      case (str, prov) =>
        Block(scitzen.sast.Attributes(Nil), SpaceComment(str), prov)
    }

  def alternatives[_p: P]: P[Sast] =
    P(extendedWhitespace |
      ListParsers.list |
      DelimitedBlockParsers.anyDelimited |
      sectionTitle |
      NoCut(MacroParsers.full) ~ spaceLine |
      paragraph)

}
