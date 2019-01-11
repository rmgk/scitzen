package scitzen.parser
import fastparse.NoWhitespace._
import fastparse._

object CommonParsers {
  def space                [_:P]= P(CharIn(" \t"))
  def newline[_:P] = P("\n")
  def eol                  [_:P]= P(newline | End)
  def iws                  [_:P]= P(CharsWhileIn(" \t", 0))
  def sws                  [_:P]= P(CharsWhileIn(" \t", 1))
  def iwsLine              [_:P]= P(iws ~ eol)
  def saws                 [_:P]= P(CharsWhileIn(" \t\n"))
  def aws                  [_:P]= P(saws.?)
  def swsLine              [_:P]= P((sws ~ End) | "\n")
  def letter               [_:P]= P(CharPred(_.isLetter)).opaque("<letter>")
  def digits               [_:P]= P(CharsWhile(_.isDigit))

  def quoted[_:P](close: String, open: String = null): P[String] = {
    P(Option(open).getOrElse(close) ~/ (("\\" ~ ("\\" | close)) | (!close ~ AnyChar)).rep.! ~/ close)
    .map { str =>
      (if (close == "\\") str else str.replace(s"\\$close", close))
      .replace("\\\\", "\\")
    }
  }

  def untilE[_:P](closing: => P[Unit], min: Int = 1):P[String] =
    P(((!closing) ~ AnyChar).rep(min).!)

  def untilI[_:P](closing: => P[Unit], min: Int = 1): P[String] =
    untilE(closing, min) ~ closing


  object Identifier {
    def charInWordList  [_:P]= P(CharIn("0-9a-zA-Z_"))
    def startIdentifier [_:P]= P(charInWordList).opaque("<start identifier>")
    def inIdentifier    [_:P]= P(CharsWhileIn("0-9a-zA-Z_\\-", 0)).opaque("<in identifier>")
    def identifier      [_:P]= P((startIdentifier ~ inIdentifier).!).opaque("<identifier>")
  }

  def identifier [_:P]=  Identifier.identifier

  def line [_:P]= P(untilE(eol, min = 0)).opaque("<line>")
}
