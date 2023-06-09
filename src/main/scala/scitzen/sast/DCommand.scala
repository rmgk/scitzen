package scitzen.sast

enum DCommand {
  case Code
  case Emph
  case Strong
  case Math
  case Cite
  case Comment
  case Def
  case Image
  case Include
  case Link
  case Ref
  case Lookup
  case Raw
  case BibQuery
  case Other(str: String)
}

object DCommand {
  val (parseMap, printMap) = {
    val standard = List(
      "bibq"    -> BibQuery,
      "cite"    -> Cite,
      "comment" -> Comment,
      "def"     -> Def,
      "image"   -> Image,
      "include" -> Include,
      "link"    -> Link,
      "ref"     -> Ref,
      "code"    -> Code,
      "emph"    -> Emph,
      "strong"  -> Strong,
      "math"    -> Math,
      "raw"     -> Raw,
      ""        -> Lookup
    )
    val aliases = Map(
      "fence" -> Include,
      "_"     -> Emph,
      "`"     -> Code,
      "*"     -> Strong,
      "$"     -> Math,
      "n"     -> Lookup
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
