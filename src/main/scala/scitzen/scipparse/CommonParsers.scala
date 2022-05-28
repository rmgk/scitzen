package scitzen.scipparse

import de.rmgk.scip.*
import scitzen.sast.Prov

import java.nio.charset.StandardCharsets
import scala.annotation.tailrec

object CommonParsers {
  inline def verticalSpaceB: Scip[Boolean]      = " \t".any
  inline def newlineB: Scip[Boolean]            = "\n".all
  val eolB: Scip[Boolean]                       = "\n".any.or(scipend)
  val verticalSpacesB: Scip[Boolean]            = " \t".any.rep.min(0)
  val significantVerticalSpacesB: Scip[Boolean] = " \t".any.rep.min(1)
  val spaceLineB: Scip[Boolean]                 = (verticalSpacesB and eolB)
  val spaceLineF: Scip[Unit]                    = spaceLineB.orFail
  val significantSpaceLineB: Scip[Boolean]      = (significantVerticalSpacesB and eolB) or newlineB
  val anySpacesB: Scip[Boolean]                 = " \t\n".any.rep.min(0)
  val anySpaces: Scip[Unit]                     = anySpacesB.orFail
  val digitsB: Scip[Boolean]                    = cpred(Character.isDigit).rep.min(1)

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
    inline def inIdentifier: Scip[Boolean]    = cpred(Character.isJavaIdentifierPart).rep.map(_ > 0)
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
