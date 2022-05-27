package scitzen.scipparse

import CompatParsers.*
import de.rmgk.scip.*


object Sciparse {
  def main(args: Array[String]): Unit = {
    println(InlineParsers("", End).full.run(using Scx("This is a :emph{triumph}! :strong{huge} success!")))
  }
}
