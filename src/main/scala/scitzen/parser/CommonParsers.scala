package scitzen.parser

import de.rmgk.scip.*
import scitzen.sast.Prov

import scala.annotation.tailrec

object CommonParsers {
  inline def verticalSpace: Scip[Boolean]      = " \t".any
  inline def newline: Scip[Boolean]            = "\n".all
  inline def eol: Scip[Boolean]                = newline or end
  val verticalSpaces: Scip[Boolean]            = verticalSpace.rep.min(0)
  val significantVerticalSpaces: Scip[Boolean] = verticalSpace.rep.min(1)
  val spaceLineB: Scip[Boolean]                = verticalSpaces and eol
  val spaceLineF: Scip[Unit]                   = spaceLineB.orFail
  val significantSpaceLine: Scip[Boolean]      = (significantVerticalSpaces and eol) or newline
  val anySpacesB: Scip[Boolean]                = " \t\n".any.rep.min(0)
  val anySpacesF: Scip[Unit]                   = anySpacesB.orFail
  val digits: Scip[Boolean]                    = cpred(Character.isDigit).rep.min(1)

  inline def untilI(inline end: Scip[Boolean]): Scip[(Int, Int)] = Scip {
    val start        = scx.index
    var lastPosition = scx.index
    @tailrec def rec(): Unit =
      if end.run then ()
      else
        scx.index = lastPosition
        if scx.next then
          lastPosition = scx.index
          rec()
        else scx.fail
    rec()
    (start, lastPosition)
  }

  inline def untilIS(inline end: Scip[Boolean]): Scip[String] = Scip {
    val res = untilI(end).run
    scx.str(res._1, res._2)
  }

  object Identifier {
    inline def startIdentifier: Scip[Boolean] = cpred(Character.isLetter)
    inline def inIdentifier: Scip[Boolean]    = cpred(Character.isJavaIdentifierPart).rep.min(0)
    inline def identifier: Scip[Boolean]      = startIdentifier.and(inIdentifier)
  }

  inline def identifierB: Scip[Boolean] = Identifier.identifier

  inline def withProv[T](inline parser: Scip[T]): Scip[(T, Prov)] = Scip {
    val s = scx.index
    val r = parser.run
    val e = scx.index
    r -> Prov(s, e)
  }
}
