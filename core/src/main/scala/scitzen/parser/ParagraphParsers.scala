package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.converter.Post
import scitzen.parser.CommonParsers._
import scribe.Loggable


sealed trait Inline
case class InlineMacro(command: String,
                       target: String,
                       attributes: Seq[Attribute])
                      (val provenance: Prov = Prov()) extends Inline {
  def withPost(post: Post) = copy()(provenance = provenance.copy(post = Some(post)))
}
case class InlineText(str: String) extends Inline
case class AttrRef(id: String) extends Inline
case class InlineQuote(q: String, inner: Seq[Inline]) extends Inline

object Inline {
  implicit val loggable: Loggable[Inline] = {
    case im : InlineMacro =>
      im.provenance.post.fold(s"$im"){post =>
        s"$im[${post.sourcePath}:${im.provenance.start} »${post.content.substring(im.provenance.start, im.provenance.end)}«]"
      }

    case other => other.toString
  }
}

object ParagraphParsers {
  //TODO: unsupported `+` for passthrough macros
  // plan: only keep macro form, less magic syntax
  // https://asciidoctor.org/docs/user-manual/#pass-macros

  def quoteChars[_: P]: P[Unit] = CharIn("_*`^~#")

  // `<` cross reference; `/` comment; `\` escape; ` `
  def otherSpecialChars[_: P]: P[Unit] = CharIn("</\\\\")

  def specialCharacter[_: P]: P[Unit] = P(quoteChars | otherSpecialChars)

  def escaped[_: P]: P[InlineText] =
    P("\\" ~ (MacroParsers.start | Attributes.reference | specialCharacter).!)
    .map(InlineText)

  /** Simple text stops at special characters or spaces.
    * Spaces are included, and then we reorient */
  def simpleText[_: P]: P[InlineText] = P(untilE(specialCharacter | anySpace)).map(InlineText)

  def special[_: P]: P[Inline] = P(escaped |
                                   comment |
                                   MacroParsers.inline |
                                   Attributes.reference |
                                   crossreference)

  def comment[_: P]: P[InlineMacro] = P(("//" ~ untilI(eol)).rep(1).!)
                                      .map(InlineMacro("//", _, Nil)())

  def crossreference[_: P]: P[InlineMacro] = P(quoted(open = "<<", close = ">>"))
                                             .map(InlineMacro("<<", _, Nil)())

  def fullParagraph[_: P]: P[Seq[Inline]] = P(inlineSequence().? ~ anySpaces ~ End)
                                            .map(_.getOrElse(Nil))

  def disable[_:P](s: String) = P(if (s.isEmpty) Pass else !s)

  def inlineSequence[_: P](currentQuote: String = ""): P[Seq[Inline]] = P {
    ((significantAnySpaces.!.map(InlineText)
      ~ disable(currentQuote)
      ~ constrainedQuote)
     | anySpaces.!.map(InlineText)
       ~ disable(currentQuote)
       ~ (special
          | unconstrainedQuote
          | simpleText
          | specialCharacter.!.map(InlineText))
    ).rep(1).map(_.flatMap { case (spaces, inline) => Seq(spaces, inline) })
  }

  def constrainedQuote[_: P]: P[InlineQuote] = P {
    quoteChars.!.flatMap { delimiter =>
      (inlineSequence(delimiter) ~ constrainedClosing(delimiter)).map(v => (delimiter, v))
    }
  }.map { case (q, inner) => InlineQuote(q, inner) }

  def constrainedClosing[_: P](wantsToClose: String): P[Unit] = P {
    wantsToClose ~ !CharPred(c => c.isUnicodeIdentifierPart)
  }

  def unconstrainedQuote[_: P]: P[InlineQuote] = P {
    quoteChars.!.flatMap { delimiterPart =>
      val delimiter = delimiterPart + delimiterPart
      delimiterPart ~ (inlineSequence(delimiter) ~ delimiter).map(v => (delimiter, v))
    }
  }.map { case (q, inner) => InlineQuote(q, inner) }

}
