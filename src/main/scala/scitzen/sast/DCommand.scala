package scitzen.sast

enum DCommand {
  case Aggregate
  case BibQuery
  case Cite
  case Code
  case Comment
  case Def
  case Emph
  case Image
  case Include
  case Index
  case Link
  case Lookup
  case Math
  case Raw
  case Ref
  case Script
  case Strong
  case Other(str: String)
}

object DCommand {
  val (parseMap, printMap) = {
    val standard = List(
      ""          -> Lookup,
      "aggregate" -> Aggregate,
      "bibquery"  -> BibQuery,
      "cite"      -> Cite,
      "code"      -> Code,
      "comment"   -> Comment,
      "def"       -> Def,
      "emph"      -> Emph,
      "image"     -> Image,
      "include"   -> Include,
      "index"     -> Index,
      "link"      -> Link,
      "math"      -> Math,
      "raw"       -> Raw,
      "ref"       -> Ref,
      "script"    -> Script,
      "strong"    -> Strong,
    )
    val aliases = Map(
      "$"     -> Math,
      "*"     -> Strong,
      "_"     -> Emph,
      "`"     -> Code,
      "fence" -> Include,
      "n"     -> Lookup,
    )

    (standard.toMap ++ aliases, standard.map(p => p._2 -> p._1).toMap)
  }
  def parseMacroCommand(str: String): DCommand = parseMap.getOrElse(str, Other(str))
  def printMacroCommand(m: DCommand): String =
    m match {
      case Other(str) => str
      case o          => printMap(o)
    }

}
