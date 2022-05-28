package scitzen.scipparse

import scitzen.sast.Prov
import CompatParsers.*
import de.rmgk.scip.*

import java.nio.charset.StandardCharsets
import scala.annotation.tailrec

object CompatParsers {

  extension (inline scip: Scip[Unit])
    inline def unary_! : Scip[Unit] = Scip {
      scip.attempt.map(!_).lookahead.orFail
    }
}

object CommonParsers {
  val verticalSpaceB: Scip[Boolean]                 = " \t".any
  val verticalSpace: Scip[Unit]                 = " \t".any.orFail
  val newlineB: Scip[Boolean]                   = "\n".all
  val newline: Scip[Unit]                       = "\n".all.orFail
  val eolB: Scip[Boolean]                       = "\n".any.or(scipend)
  val eol: Scip[Unit]                           = eolB.orFail
  val verticalSpacesB: Scip[Boolean]            = " \t".any.rep.min(0)
  val verticalSpaces: Scip[Unit]                = verticalSpacesB.orFail
  val significantVerticalSpacesB: Scip[Boolean] = " \t".any.rep.min(1)
  val significantVerticalSpaces: Scip[Unit]     = significantVerticalSpacesB.orFail
  val spaceLineB: Scip[Boolean]                 = (verticalSpacesB and eolB)
  val spaceLine: Scip[Unit]                     = spaceLineB.orFail
  val significantSpaceLineB: Scip[Boolean]      = (significantVerticalSpacesB and eolB) or newlineB
  val significantSpaceLine: Scip[Unit]          = significantSpaceLineB.orFail
  val anySpaces: Scip[Unit]                     = " \t\n".any.rep.min(0).orFail
  val digitsB: Scip[Boolean]                    = cpred(Character.isDigit).rep.min(1)
  val digits: Scip[Unit]                        = digitsB.orFail

  def untilE(closing: Scip[Unit], min: Int = 1): Scip[String] = Scip {
    val start = scx.index
    until(closing.attempt).run
    val count = scx.index - start
    if count < min then scx.fail
  }.dropstr

  inline def untilI(inline end: Scip[Boolean]): Scip[String] = Scip {
    val start        = scx.index
    var lastPosition = scx.index
    @tailrec def rec(): Unit =
      if end.run then ()
      else
        scx.index = lastPosition
        if scx.next then
          lastPosition = scx.index
          rec()
        else ()
    rec()
    scx.str(start, lastPosition)
  }

  object Identifier {
    inline def startIdentifier: Scip[Boolean] = cpred(Character.isLetter)
    inline def inIdentifier: Scip[Boolean]    = cpred(Character.isJavaIdentifierPart).rep.map(_ > 0)
    inline def identifier: Scip[Boolean]      = startIdentifier.and(inIdentifier)
  }

  val identifierB: Scip[Boolean] = Identifier.identifier
  val identifier: Scip[Unit] = Identifier.identifier.orFail

  def withProv[T](parser: Scip[T]): Scip[(T, Prov)] = Scip {
    val s = scx.index
    val r = parser.run
    val e = scx.index
    r -> Prov(s, e)
  }
}
