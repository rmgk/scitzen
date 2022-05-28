package scitzen.scipparse

import scitzen.sast.Prov
import CompatParsers.*
import de.rmgk.scip.*

import java.nio.charset.StandardCharsets

object CompatParsers {

  extension (inline scip: Scip[Unit])
    inline def unary_! : Scip[Unit] = Scip {
      scip.attempt.map(!_).lookahead.orFail
    }
  inline def End: Scip[Unit] = scipend.orFailWith("not end of line")

}

object CommonParsers {
  val verticalSpace: Scip[Unit]                 = " \t".any.orFailWith("space")
  val newlineB: Scip[Boolean]                   = "\n".scip
  val newline: Scip[Unit]                       = "\n".scip.orFail
  val eolB: Scip[Boolean]                       = "\n".any.or(scipend)
  val eol: Scip[Unit]                           = eolB.orFail
  val verticalSpacesB: Scip[Boolean]            = " \t".any.rep.min(0)
  val verticalSpaces: Scip[Unit]                = verticalSpacesB.orFail
  val significantVerticalSpacesB: Scip[Boolean] = " \t".any.rep.min(1)
  val significantVerticalSpaces: Scip[Unit]     = significantVerticalSpacesB.orFail
  val spaceLineB: Scip[Boolean]                     = (verticalSpacesB and eolB)
  val spaceLine: Scip[Unit]                     = spaceLineB.orFail
  val significantSpaceLineB: Scip[Boolean]      = (significantVerticalSpacesB and eolB) or newlineB
  val significantSpaceLine: Scip[Unit]          = significantSpaceLineB.orFail
  val anySpaces: Scip[Unit]                     = " \t\n".any.rep.min(0).orFail
  val digits: Scip[Unit]                        = cpred(Character.isDigit).rep.min(1).orFail

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
    inline def startIdentifier: Scip[Boolean] = cpred(Character.isLetter)
    inline def inIdentifier: Scip[Boolean]    = cpred(Character.isJavaIdentifierPart).rep.map(_ > 0)
    inline def identifier: Scip[Boolean]      = startIdentifier.and(inIdentifier)
  }

  val identifier: Scip[Unit] = Identifier.identifier.orFail

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
