package scitzen.parser3

import cats.parse.{LocationMap, Parser}
import cats.parse.Parser.Expectation
import cats.parse.Parser.Error

class Prettyprint(filename: String, input: String) {
  val locmap = LocationMap(input)

  def description(x: Expectation): String = x match {
    case Expectation.OneOfStr(_, strs) =>
      val strList = strs.map { x => s"'$x'" }.mkString(", ")
      s"expected one of $strList"
    case Expectation.InRange(_, lower, upper) =>
      if (lower == upper) s"expected '$lower'"
      else s"expected '$lower' ~ '$upper'"
    case Expectation.StartOfString(_) =>
      "expected beginning of file"
    case Expectation.EndOfString(_, _) =>
      "expected end of file"
    case Expectation.Length(_, expected, actual) =>
      s"unexpected eof; expected ${expected - actual} more characters"
    case Expectation.ExpectedFailureAt(_, matched) =>
      s"unexpected '$matched'"
    case Expectation.Fail(_) =>
      "failed to parse"
    case Expectation.FailWith(_, message) =>
      message
    case Expectation.WithContext(contextStr, nested) =>
      s"$contextStr > ${description(nested)}"
  }

  def prettyprint(x: Expectation): String = {
    val (row, col) = locmap.toLineCol(x.offset).get
    val (r, c)     = (row + 1, col + 1)
    val line       = locmap.getLine(row).get
    val offending =
      s"${row.toString map { _ => ' ' }} | ${" " * col}^"
    val main = s"""
      |$filename:$r:$c: error: ${description(x)}
      |$r | $line
      |$offending""".stripMargin
    main
    //x match {
    //  case Expectation.WithContext(contextStr, expect) =>
    //    val sub = prettyprint(expect).indent(2)
    //    s"$main$sub"
    //  case _ => main
    //}
  }

  def prettyprint(x: Error): String =
    x.expected.map(prettyprint).toList.mkString("")
}
