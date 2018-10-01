package asciimedic

import asciimedic.CommonParsers._
import fastparse.all._


sealed trait Inline
case class InlineMacro(command: String, target: String, attributes: Seq[Attribute]) extends Inline
case class InlineText(str: String) extends Inline
case class AttrRef(id: String) extends Inline
case class InlineQuote(q: String, inner: Seq[Inline]) extends Inline
case class InlineComment(text: String) extends Inline

object ParagraphParsers {
  val quoteChars: String = "_*`^~#"

  val specialCharacter = P(CharIn(quoteChars ++ "/\\"))
  val constrainedQuote = P(CharIn(quoteChars))
  val escaped          = P("\\" ~ (Macros.start | Attributes.reference).!)
                         .map(InlineText)

  val text = P(untilE(specialCharacter))
             .map(InlineText)


  case class InnerParser[Res](outer: Seq[Char] = Seq.empty) {


    val quotes: Parser[InlineQuote] = P {
      quoteChars.filterNot(outer.contains).flatMap(c => Seq(s"$c", s"$c$c")).map { delimiter =>
        delimiter.! ~ InnerParser(outer :+ delimiter.head).inlineSequence ~ delimiter
      }.reduce(_ | _)
    }.map { case (q, inner) => InlineQuote(q, inner) }

    //

    val comment = P(("//" ~ untilI(eol)).rep(min = 1).!)
                  .map(InlineComment)


    val special: Parser[Inline] = P(escaped |
                                    comment |
                                    Macros.urls.url |
                                    Macros.inline |
                                    Attributes.reference |
                                    quotes)

    val inlineSequence: Parser[Seq[Inline]] = P((text ~ special.?).rep(min = 1)).log()
                                              .map(ts => ts.flatMap { case (t, s) => Seq(t) ++ s })

    val fullParagraph = P(inlineSequence ~ End)
  }

}
