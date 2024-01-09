package sciparse

import scala.quoted.*

inline def printCode[T](inline expr: T): String =
  ${ impl('expr) }

def impl[T: Type](expr: Expr[T])(using quotes: Quotes): Expr[String] = {

  import quotes.reflect.*
  val e = expr.show.toString
  // val t = expr.asTerm.toString
  // println(e)
  // println(t)
  Expr(e)

}
