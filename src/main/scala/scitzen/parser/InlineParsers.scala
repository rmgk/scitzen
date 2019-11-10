package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._


object InlineParsers {
  //TODO: unsupported `+` for passthrough macros
  // plan: only keep macro form, less magic syntax
  // https://asciidoctor.org/docs/user-manual/#pass-macros

  // also do not support super ^ subscript ~ or span #


  def quoteChars[_: P]: P[Unit] = CharIn("_*`$")
  def commentStart[_: P]: P[Unit] = P(":%")

  def specialChars[_: P]: P[Unit] = CharIn("_*`$%")


  def syntaxStart[_: P]: P[Unit] = P(CharIn(":") ~ ( specialChars | MacroParsers.detectStart) )

  // grab everything until a unconstrained position followed by a syntax starter
  // include the unconstrained position
  // the until fails if empty, in that was we are just now at a potential syntax start,
  // so eat that and return
  private def notSyntax[_: P]: P[String] = P(untilE(End | syntaxStart))

  def simpleText[_: P]: P[InlineText] = {
    P(notSyntax.!).map(InlineText)
  }

  def comment[_: P]: P[Macro] = P(commentStart ~ untilI(eol, 0))
                                .map(text => Macro("comment", List(Attribute("", text))))

  def fullParagraph[_: P]: P[Seq[InlineProv]] =
    P(inlineSequence.? ~ End)
    .map(_.getOrElse(Nil))

  def inlineSequence[_: P]: P[Seq[InlineProv]] = P {
    (Index ~ (comment
     | MacroParsers.full
     | quoted
     | simpleText
    ) ~ Index).map{case (s, i, e) =>
      val prov = implicitly[P[_]].misc.getOrElse("provenanceOffset", Prov(0,0)).asInstanceOf[Prov]
      InlineProv(i, Prov(prov.start + s, prov.start + e))
    }.rep(1)
  }

  def quoted[_: P]: P[InlineQuote] = P {
    CharIn(":") ~ quoteChars.!.flatMap { delimiter =>
      (!(delimiter | anySpace) ~ untilE(delimiter))
      .!.map(v => InlineQuote(delimiter, v)) ~ delimiter
    }
  }

}
