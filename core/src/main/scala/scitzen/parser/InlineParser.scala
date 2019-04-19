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
case class InlineQuote(q: String, inner: String) extends Inline

object Inline {
  implicit val loggable: Loggable[Inline] = {
    case im: InlineMacro =>
      im.provenance.post.fold(s"$im") { post =>
        s"$im[${post.sourcePath}:${im.provenance.start} »${
          post.content.substring(im.provenance.start,
                                 im.provenance.end)
        }«]"
      }

    case other => other.toString
  }
}

object InlineParser {
  //TODO: unsupported `+` for passthrough macros
  // plan: only keep macro form, less magic syntax
  // https://asciidoctor.org/docs/user-manual/#pass-macros

  // also do not support super ^ subscript ~ or span #


  def quoteChars[_: P]: P[Unit] = CharIn("_*`$")

  // `<` cross reference; `/` comment; `\` escape; ` `
  def otherSpecialChars[_: P]: P[Unit] = CharIn("/")

  def specialCharacter[_: P]: P[Unit] = P(quoteChars | otherSpecialChars)

  def allowSyntaxAfter[_: P]: P[Unit] = P(anySpace | "(")
  def syntaxStart[_: P]: P[Unit] = P(specialCharacter | Identifier.startIdentifier)

  // grab everything until a unconstrained position followed by a syntax starter
  // include the unconstrained position
  // the until fails if empty, in that was we are just now at a potential syntax start,
  // so eat that and return
  private def notSyntax[_: P]: P[Unit] = P((untilE(End | "//" | allowSyntaxAfter ~ &(syntaxStart))
                                            ~/ (allowSyntaxAfter | &("//") | End)).map(_ => ())
                                           | allowSyntaxAfter)
  def simpleText[_: P]: P[InlineText] = {
    P(notSyntax.!).map(InlineText)
  }

  def comment[_: P]: P[InlineMacro] = P("//" ~ untilI(eol, 0))
                                      .map(InlineMacro("//", _, Nil)())

  def fullParagraph[_: P]: P[Seq[Inline]] = P(inlineSequence.? ~ End)
                                            .map(_.getOrElse(Nil))

  def inlineSequence[_: P]: P[Seq[Inline]] = P {
    (comment
     | MacroParsers.inline
     | quoted
     | simpleText
    ).rep(1)
  }

  def quoted[_: P]: P[InlineQuote] = P {
    quoteChars.!.flatMap { delimiter =>
      (!(delimiter | anySpace) ~ untilE(delimiter))
      .!.map(v => InlineQuote(delimiter, v)) ~ delimiter
    }
  }

}
