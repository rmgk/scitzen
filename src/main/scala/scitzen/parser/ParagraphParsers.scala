package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.parser.CommonParsers._


sealed trait Inline
case class InlineMacro(command: String, target: String, attributes: Seq[Attribute]) extends Inline
case class InlineText(str: String) extends Inline
case class AttrRef(id: String) extends Inline
case class InlineQuote(q: String, inner: Seq[Inline]) extends Inline

object ParagraphParsers {
  //TODO: unsupported `+` for passthrough macros
  // plan: only keep macro form, less magic syntax
  // https://asciidoctor.org/docs/user-manual/#pass-macros

  def quoteChars[_:P]: P[Unit] = CharIn("_*`^~#")

  // `<` cross reference; `/` comment; `\` escape; ` `
  def otherSpecialChars [_:P]: P[Unit] = CharIn("</\\\\")

  def specialCharacter [_:P]: P[Unit] = P(quoteChars | otherSpecialChars)

  def escaped          [_:P]: P[InlineText] = P("\\" ~ (MacroParsers.start | Attributes.reference | specialCharacter).!)
                                              .map(InlineText)

  /** Simple text stops at special characters or spaces.
    * Spaces are included, and then we reorient*/
  def simpleText [_:P]: P[InlineText] = P(untilE(specialCharacter | anySpace)).map(InlineText)

  def special[_:P]: P[Inline] = P(escaped |
                                  comment |
                                  MacroParsers.urls.url |
                                  MacroParsers.inline |
                                  Attributes.reference |
                                  crossreference )

  def comment [_:P]: P[InlineMacro] = P(("//" ~ untilI(eol)).rep(1).!)
                                      .map(InlineMacro("//", _, Nil))

  def crossreference [_:P]: P[InlineMacro] = P(quoted(open = "<<", close = ">>"))
                                             .map(InlineMacro("<<", _, Nil))

  def fullParagraph [_:P]: P[Seq[Inline]] = P(InnerParser().inlineSequence ~ End)



case class InnerParser[Res](wantToClose: String = "", constrained: Boolean = false) {


    def quotes[_:P](rep: Int): P[InlineQuote] = P {
        quoteChars.rep(min = rep, max = rep).!.flatMap { delimiter =>
          InnerParser(delimiter, delimiter.length == 1).inlineSequence.map(v => (delimiter, v))
        }
    }.map { case (q, inner) => InlineQuote(q, inner) }

    def closing[_:P]: P[Unit] = P{
      if (!constrained) wantToClose ~ wantToClose
      else wantToClose ~ !CharPred(c => c.isLetterOrDigit || c == '_')
    }



    def inlineSequence[_:P]: P[Seq[Inline]] = P {
      if (constrained) {
        ((significantAnySpaces ~ quotes(1)) | anySpaces ~ (special | quotes(2) | simpleText | specialCharacter.!.map(InlineText))).rep(0) ~ closing
      }
      else {
        ((significantAnySpaces ~ quotes(1)) | anySpaces ~ (special | quotes(2) | simpleText | specialCharacter.!.map(InlineText))).rep(0) ~ anySpaces ~ closing
      }
    }

  }

}
