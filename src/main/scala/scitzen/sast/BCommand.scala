package scitzen.sast

enum BCommand {
  case Empty
  case Convert
  case Code
  case Figure
  case Embed
  case If
  case Other(str: String)
}

object BCommand {
  val (parseMap, printMap) = {
    val standard = List(
      "convert" -> Convert,
      "code" -> Code,
      "figure" -> Figure,
      "embed" -> Embed,
      "if" -> If,
      "" -> Empty
    )
    val aliases = Map()

    (standard.toMap ++ aliases, standard.map(p => p._2 -> p._1).toMap)
  }
  def parse(str: String): BCommand = parseMap.getOrElse(str, Other(str))
  def print(m: BCommand): String =
    m match {
      case Other(str) => str
      case o          => printMap(o)
    }

}
