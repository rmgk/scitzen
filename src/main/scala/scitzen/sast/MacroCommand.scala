package scitzen.sast


sealed trait MacroCommand
object MacroCommand {
  val (parseMap, printMap) = {
    val standard = List(
      "cite"    -> Cite,
      "comment" -> Comment,
      "def"     -> Def,
      "image"   -> Image,
      "include" -> Include,
      "label"   -> Label,
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
  def parseMacroCommand(str: String): MacroCommand = parseMap.getOrElse(str, Other(str))
  def printMacroCommand(m: MacroCommand): String =
    m match {
      case Other(str) => str
      case o          => printMap(o)
    }

  object Code                   extends MacroCommand
  object Emph                   extends MacroCommand
  object Strong                 extends MacroCommand
  object Math                   extends MacroCommand
  object Cite                   extends MacroCommand
  object Comment                extends MacroCommand
  object Def                    extends MacroCommand
  object Image                  extends MacroCommand
  object Include                extends MacroCommand
  object Label                  extends MacroCommand
  object Link                   extends MacroCommand
  object Ref                    extends MacroCommand
  object Lookup                 extends MacroCommand
  case class Other(str: String) extends MacroCommand
}
