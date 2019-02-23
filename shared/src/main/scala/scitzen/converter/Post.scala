package scitzen.converter


import scitzen.parser.{DateParsingHelper, Document, ScitzenDateTime}


class Post(val document: Document, val targetPath: String) {
  val attributes: Map[String, String] = document.header.get.attributes.map(a => a.id -> a.value).toMap
  def commaSeparatedAttribute(key: String): List[String] =
    attributes.getOrElse(key, "").toString
    .split(',')
    .map[String, List[String]](_.trim)(collection.breakOut)
    .filter(_.nonEmpty)

  def people(): List[String] = commaSeparatedAttribute("people")

  def folder(): Option[String] = attributes.get("folder")

  def categories(): List[String] = commaSeparatedAttribute("categories")

  def title: String = document.header.fold("(null)")(_.title)
  lazy val date: Option[ScitzenDateTime] = attributes.get("revdate").map(v => DateParsingHelper.parseDate(v.trim))
  lazy val modified: Option[ScitzenDateTime] = attributes.get("modified")
                                               .map(m => DateParsingHelper.parseDate(m.trim))


}

