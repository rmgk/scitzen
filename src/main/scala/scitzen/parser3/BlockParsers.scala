package scitzen.parser3

import cats.parse.{Numbers, Rfc5234, Parser as P, Parser0 as P0}
import cats.syntax.*
import cats.implicits.*
import cats.parse.Numbers.digits
import cats.parse.Rfc5234.sp
import cats.parse.Parser.*
import CommonParsers.*
import scitzen.parser.{DelimitedBlockParsers, ListParsers}
import scitzen.sast.{Attribute, Attributes, Block, Inline, Paragraph, Prov, Sast, Section, SpaceComment, Text}

object BlockParsers {

  val paragraphInlines = InlineParsers.full((eol ~ spaceLine).void, allowEmpty = false)

  val sectionInlines = InlineParsers.full(eol, allowEmpty = false)

  val paragraph: P[Block] =
    ((
      (scitzen.parser3.AttributesParser.braces <* spaceLine).?.with1 ~
        withProv(paragraphInlines)
    ).map {
      case (attrOpt, ((inlines, _), prov)) =>
        //val endline = if (end.contains('\n')) inlines :+ InlineText("\n") else inlines
        Block(scitzen.sast.Attributes(attrOpt.getOrElse(Nil)), Paragraph(Text(inlines)), prov)
    })

  val sectionStart: P[String] = (charIn("=#").rep.string <* " ")
  val sectionTitle: P[Section] =
    (sectionStart
      ~ withProv(sectionInlines)
      ~ (scitzen.parser3.AttributesParser.braces <* spaceLine | scitzen.parser3.AttributesParser.noBraces).?)
      .map { (data: (((String, ((Seq[scitzen.sast.Inline], String), scitzen.sast.Prov)), Option[List[scitzen.sast.Attribute]]))) =>
        val (((prefix: String, ((inlines: List[Inline], _), prov: Prov)), attrl: Option[List[Attribute]])) = data
          //val inlines = Parse.inlineUnwrap(inl, prov)
          Section(scitzen.sast.Text(inlines), prefix, Attributes(attrl.getOrElse(Nil)), prov)
      }

  val extendedWhitespace: P[Block] =
    (withProv(
      (significantSpaceLine.rep(1) |
       scitzen.parser3.MacroParsers.comment.rep(1)).rep(1).string
    )).map {
      case (str, prov) =>
        Block(scitzen.sast.Attributes(Nil), SpaceComment(str), prov)
    }

  val alternatives: P[Sast] =
    (extendedWhitespace.backtrack.withContext("whitespace") |
      scitzen.parser3.ListParsers.list.backtrack.withContext("list") |
      scitzen.parser3.DelimitedBlockParsers.anyDelimited.backtrack.withContext("delimited block") |
      sectionTitle.backtrack.withContext("section") |
     (scitzen.parser3.MacroParsers.full <* spaceLine).backtrack.withContext("macro") |
      paragraph.withContext("paragraph"))

}
