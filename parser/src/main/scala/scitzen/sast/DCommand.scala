package scitzen.sast

sealed trait DCommand
object DCommand {
  val (parseMap, printMap) = {
    val standard = List(
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

  object Code extends DCommand
  object Emph extends DCommand
  object Strong extends DCommand
  object Math extends DCommand
  object Cite extends DCommand
  object Comment extends DCommand
  object Def extends DCommand
  object Image extends DCommand
  object Include extends DCommand
  object Link extends DCommand
  object Ref extends DCommand
  object Lookup extends DCommand
  case class Other(str: String) extends DCommand
}
