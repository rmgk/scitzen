package scitzen.parser

import fastparse.NoWhitespace._
import fastparse._
import scitzen.sast.Prov

object CommonParsers {
  def verticalSpace[_: P]: P[Unit]             = P(CharIn(" \t"))
  def newline[_: P]: P[Unit]                   = P("\n")
  def anySpace[_: P]: P[Unit]                  = P(CharIn(" \t\n"))
  def eol[_: P]: P[Unit]                       = P(newline | End)
  def verticalSpaces[_: P]: P[Unit]            = P(CharsWhileIn(" \t", 0))
  def significantVerticalSpaces[_: P]: P[Unit] = P(CharsWhileIn(" \t", 1))
  def spaceLine[_: P]: P[Unit]                 = P(verticalSpaces ~ eol)
  def significantSpaceLine[_: P]: P[Unit]      = P((significantVerticalSpaces ~ eol) | newline)
  def anySpaces[_: P]: P[Unit]                 = P(CharsWhileIn(" \t\n", 0))
  def significantAnySpaces[_: P]: P[Unit]      = P(CharsWhileIn(" \t\n", 1))
  def letter[_: P]: P[Unit]                    = P(CharPred(_.isLetter)).opaque("<letter>")
  def digits[_: P]: P[Unit]                    = P(CharsWhile(_.isDigit))

  def quoted[_: P](close: String, open: String = null): P[String] = {
    P(anySpaces ~ Option(open).getOrElse(close) ~/ (("\\" ~ ("\\" | close)) | (!close ~ AnyChar)).rep.! ~/ close)
      .map { str =>
        (if (close == "\\") str else str.replace(s"\\$close", close))
          .replace("\\\\", "\\")
      }
  }

  def untilParagraphEnd[_: P] = {
    P((!(eol ~ spaceLine) ~ AnyChar ~ CharsWhile(c => c != '\n', 0)).rep(1).!)
  }
  def untilE[_: P](closing: => P[Unit], min: Int = 1): P[String] =
    P(((!closing) ~ AnyChar).rep(min).!)

  def untilI[_: P](closing: => P[Unit], min: Int = 0): P[String] =
    untilE(closing, min) ~ closing

  object Identifier {
    def startIdentifier[_: P]: P[Unit] = P(CharPred(Character.isLetter)).opaque("<start identifier>")
    def inIdentifier[_: P]: P[Unit]    = P(CharsWhile(Character.isJavaIdentifierPart, 0)).opaque("<in identifier>")
    def identifier[_: P]: P[Unit]      = P(startIdentifier ~ inIdentifier).opaque("<identifier>")
  }

  def identifier[_: P]: P[Unit] = Identifier.identifier

  def line[_: P]: P[String] = P(untilE(eol, min = 0)).opaque("<line>")

  def withProv[T, _: P](parser: => P[T]): P[(T, Prov)] =
    P(Index ~ parser ~ Index).map { case (s, r, e) => r -> withOffset(s, e) }

  def withOffset(s: Int, e: Int)(implicit p: P[_]): Prov = {
    val prov = p.misc.getOrElse("provenanceOffset", Prov(0, 0)).asInstanceOf[Prov]
    Prov(prov.start + s, prov.start + e)
  }
}
