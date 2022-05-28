package scitzen.scipparse

import scitzen.sast.Prov
import CompatParsers.*
import de.rmgk.scip.*

import java.nio.charset.StandardCharsets

object CompatParsers {

  extension (inline scip: Scip[Unit]) inline def unary_! : Scip[Unit] = Scip {
    scip.attempt.map(!_).lookahead.falseFail("")
  }



  inline def End: Scip[Unit] = Scip {
    scx.index == scx.input.length || scx.fail("not end of line")
  }


  inline def CharPred(inline p: Int => Boolean): Scip[Unit] =
    cpred(p).falseFail(s"pred did not match")

  extension (inline scip: Scip[Boolean]) {
    inline def falseFail(msg: => String): Scip[Unit] = Scip {
      scip.run match
        case true  => ()
        case false => scx.fail(msg)
    }

    inline def or(inline other: Scip[Boolean]): Scip[Boolean] = Scip { scip.run || other.run }
    inline def and(inline other: Scip[Boolean]): Scip[Boolean] = Scip { scip.run && other.run }
  }

  inline def CharsWhile(inline p: Int => Boolean, min: Int) =
    cpred(p).rep.require(_ >= min).drop

}

object CommonParsers {
  val verticalSpace: Scip[Unit]             = " \t".any.falseFail("space")
  val newline: Scip[Unit]                   = "\n".scip
  val eol: Scip[Unit]                       = choice(newline, End)
  val verticalSpaces: Scip[Unit]            = " \t".any.rep.drop
  val significantVerticalSpaces: Scip[Unit] = " \t".any.rep.require(_ >= 1).drop
  val spaceLine: Scip[Unit]                 = verticalSpaces ~ eol
  val significantSpaceLine: Scip[Unit]      = choice(significantVerticalSpaces ~ eol, newline)
  val anySpaces: Scip[Unit]                 = " \t\n".any.rep.drop
  val digits: Scip[Unit]                    = cpred(Character.isDigit).rep.require(_ > 0).drop

  def untilE(closing: Scip[Unit], min: Int = 1): Scip[String] = Scip {
    val start = scx.index
    until(closing.attempt).run
    val count = scx.index - start
    if count < min then scx.fail(s"only matched $count times, not $min")
  }.str

  def untilI(closing: Scip[Unit]): Scip[String] = Scip {
    val res = untilE(closing, 0).run
    closing.run
    res
  }

  object Identifier {
    val startIdentifier: Scip[Unit] = CharPred(Character.isLetter)
    val inIdentifier: Scip[Unit]    = CharsWhile(Character.isJavaIdentifierPart, 0)
    val identifier: Scip[Unit]      = startIdentifier ~ inIdentifier
  }

  val identifier: Scip[Unit] = Identifier.identifier

  def withProv[T](parser: Scip[T]): Scip[(T, Prov)] = Scip {
    val s = scx.index
    val r = parser.run
    val e = scx.index
    r -> withOffset(s, e)
  }

  def withOffset(s: Int, e: Int): Prov = {
    // val prov = p.misc.getOrElse("provenanceOffset", Prov(0, 0)).asInstanceOf[Prov]
    val prov = Prov(0, 0)
    Prov(prov.start + s, prov.start + e)
  }
}
