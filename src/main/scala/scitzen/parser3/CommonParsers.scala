package scitzen.parser3

import cats.parse.{Numbers, Rfc5234, Parser as P, Parser0 as P0}
import cats.syntax.*
import cats.implicits.*
import cats.parse.Numbers.digits
import cats.parse.Rfc5234.sp
import cats.parse.Parser.*
import scitzen.sast.Prov

import scala.util.DynamicVariable

object CommonParsers {

  implicit def stringParser(str: String): P[Unit] = string(str)

  val verticalSpace: P[Unit]             = Rfc5234.wsp
  val newline: P[Unit]                   = char('\n')
  val anySpace: P[Unit]                  = charIn("\n\t ").void
  val eol: P0[Unit]                      = newline.orElse(end)
  val verticalSpaces: P0[Unit]           = verticalSpace.rep0.void
  val significantVerticalSpaces: P[Unit] = verticalSpace.rep.void
  val spaceLine: P0[Unit]                = (verticalSpaces ~ eol).void
  val significantSpaceLine: P[Unit]      = (newline orElse (significantVerticalSpaces ~ eol)).void.backtrack.withContext("significant space line")
  val anySpaces: P0[Unit]                = anySpace.rep0.void
  val significantAnySpaces: P[Unit]      = anySpace.rep.void
  val digits: P[Unit]                    = charWhere(_.isDigit).rep.void

  val commentStart: P[Unit] = ":%"
  val attrOpen              = "{"
  val attrClose             = "}"


  def untilI(closing: P[Unit]): P[String] =
    until0(closing).with1 <* closing

  def restOfLine[R](start: P[R]): P[(R, String)] =
    start ~ (until(eol) ~ eol).string

  object Identifier {
    val startIdentifier: P[Unit] = charsWhile(Character.isLetter).void
    val inIdentifier: P0[Unit]   = charsWhile0(Character.isJavaIdentifierPart).void
    val identifier: P[Unit]      = (startIdentifier ~ inIdentifier).void
  }

  val identifier: P[Unit] = Identifier.identifier

  val macroStart : P[Unit] = (":" ~ identifier.? ~ attrOpen).void
  val syntaxStart: P[Unit] = commentStart | macroStart

  def withProv[T](parser: P[T]): P[(T, Prov)] =
    (index.with1 ~ parser ~ index).map { case ((s, r), e) => r -> withOffset(s, e) }

  val providenceOffset = new DynamicVariable[Prov](Prov(0, 0))

  def withOffset(s: Int, e: Int): Prov = {
    val prov = providenceOffset.value
    Prov(prov.start + s, prov.start + e)
  }
}
