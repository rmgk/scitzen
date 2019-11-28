package scitzen.generic

import scala.util.matching.Regex

object RegexContext {
  implicit class regexStringContext(val sc: StringContext) {
    object rex {
      def unapplySeq(m: String): Option[Seq[String]] = {
        val regex = new Regex(sc.parts.mkString(""))
        regex.findFirstMatchIn(m).map { gs =>
          gs.subgroups
        }
      }
    }
  }
}
