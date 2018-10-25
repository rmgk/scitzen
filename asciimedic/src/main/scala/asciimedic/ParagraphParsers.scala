package asciimedic

import asciimedic.CommonParsers._
import fastparse._; import fastparse.NoWhitespace._


sealed trait Inline
case class InlineMacro(command: String, target: String, attributes: Seq[Attribute]) extends Inline
case class InlineText(str: String) extends Inline
case class AttrRef(id: String) extends Inline
case class InlineQuote(q: String, inner: Seq[Inline]) extends Inline

object ParagraphParsers {
  def quoteChars[_:P]: P[Unit] = CharIn("_*`^~#")

  def otherSpecialChars [_:P]= CharIn("</\\\\")

  def specialCharacter [_:P]= P(quoteChars | otherSpecialChars)
  def constrainedQuote [_:P]= P(quoteChars)
  def escaped          [_:P]= P("\\" ~ (MacroParsers.start | Attributes.reference).!)
                         .map(InlineText)

  def text [_:P]= P(untilE(specialCharacter))
             .map(InlineText)


  case class InnerParser[Res](outer: Seq[Char] = Seq.empty) {


    def quotes[_:P]: P[InlineQuote] = P {
      !CharPred(outer.contains) ~
        quoteChars.rep(min = 1, max = 2).!.flatMap { delimiter =>
          (InnerParser(outer :+ delimiter.head).inlineSequence ~ delimiter).map(v => (delimiter, v))
        }
    }.map { case (q, inner) => InlineQuote(q, inner) }

    def comment [_:P]= P(("//" ~ untilI(eol)).rep(1).!)
                  .map(InlineMacro("//", _, Nil))

    def crossreference [_:P]= P(quoted(open = "<<", close = ">>"))
                         .map(InlineMacro("<<", _, Nil))


    def special[_:P]: P[Inline] = P(escaped |
                                    comment |
                                    MacroParsers.urls.url |
                                    MacroParsers.inline |
                                    Attributes.reference |
                                    crossreference |
                                    quotes |
                                    AnyChar.!.map(InlineText))

    def inlineSequence[_:P]: P[Seq[Inline]] = P((text ~ special.?).rep(1)).log
                                              .map(ts => ts.flatMap { case (t, s) => Seq(t) ++ s })

    def fullParagraph [_:P]= P(inlineSequence ~ End)
  }

}
