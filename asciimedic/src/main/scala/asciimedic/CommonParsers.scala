package asciimedic

import fastparse.all._

object CommonParsers {
  val whitespaceCharacters = " \t"
  val newline              = "\n"
  val space                = P(CharIn(whitespaceCharacters))
  val eol                  = P(newline | End)
  val iws                  = P(CharsWhileIn(whitespaceCharacters, min = 0))
  val sws                  = P(CharsWhileIn(whitespaceCharacters, min = 1))
  val iwsLine              = P(iws ~ eol)
  val saws                 = P(CharsWhileIn(whitespaceCharacters ++ newline))
  val aws                  = P(saws.?)
  val swsLine              = P((sws ~ End) | newline)
  val letter               = P(CharPred(_.isLetter)).opaque("<letter>")
  val digits               = P(CharsWhile(_.isDigit))

  def quoted(close: String, open: String = null): Parser[String] = {
    P(Option(open).getOrElse(close) ~/ (("\\" ~ ("\\" | close)) | (!close ~ AnyChar)).rep.! ~/ close)
    .map { str =>
      (if (close == "\\") str else str.replace(s"\\$close", close))
      .replace("\\\\", "\\")
    }
  }

  def untilE(closing: Parser[Unit], content: Parser[Unit] = AnyChar, min: Int = 1):Parser[String] =
    P(((!closing) ~ content).rep(min).!)

  def untilI(closing: Parser[Unit], content: Parser[Unit] = AnyChar, min: Int = 1): Parser[String] =
    P(untilE(closing, content, min) ~ closing)


  object Identifier {
    val charInWordList  = ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z') ++ "_"
    val startIdentifier = P(CharIn(charInWordList)).opaque("<start identifier>")
    val inIdentifier    = P(CharsWhileIn(charInWordList ++ "-", 0)).opaque("<in identifier>")
    val identifier      = P((startIdentifier ~ inIdentifier).!).opaque("<identifier>")
  }

  val identifier =  Identifier.identifier

  val line = P(untilE(eol, min = 0)).opaque("<line>")
}
