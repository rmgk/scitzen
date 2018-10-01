package asciimedic

import fastparse.all._

object CommonParsers {
  val newlineCharacter     = "\n"
  val whitespaceCharacters = " \t"
  val eol                  = P(newlineCharacter | End)
  val iws                  = P(CharsWhileIn(whitespaceCharacters, min = 0))
  val sws                  = P(CharsWhileIn(whitespaceCharacters, min = 1))
  val iwsLine              = P(iws ~ eol)
  val saws                 = P(CharsWhileIn(whitespaceCharacters ++ newlineCharacter))
  val aws                  = P(saws.?)
  val swsLine              = P((sws ~ End) | newlineCharacter)
  val letter               = P(CharPred(_.isLetter)).opaque("<letter>")

  def quoted(close: String, open: Option[String] = None): Parser[String] = {
    P(open.getOrElse(close) ~/ (("\\" ~ ("\\" | close)) | (!close ~ AnyChar)).rep.! ~/ close)
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
