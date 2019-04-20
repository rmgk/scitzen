package scitzen.converter


import scitzen.parser.{DateParsingHelper, Document, ScitzenDateTime, SectionTitle}


case class Post(val document: Document,
           val targetPath: String,
           val sourcePath: String,
           val content: String,
           val biblio: Map[String, String] = Map()) {
  lazy val attributes: Map[String, String] = document.named
  def commaSeparatedAttribute(key: String): List[String] =
    attributes.getOrElse(key, "").toString
    .split(',')
    .map[String, List[String]](_.trim)(collection.breakOut)
    .filter(_.nonEmpty)

  def people(): List[String] = commaSeparatedAttribute("people")

  def folder(): Option[String] = attributes.get("folder")

  def categories(): List[String] = commaSeparatedAttribute("categories")

  def title: String = document.blocks.iterator.map(_.content)
                      .collectFirst { case SectionTitle(level, title) => title }.getOrElse("")
  lazy val date    : Option[ScitzenDateTime] = attributes.get("revdate").map(v => DateParsingHelper.parseDate(
    v.trim))
  lazy val modified: Option[ScitzenDateTime] = attributes.get("modified")
                                               .map(m => DateParsingHelper.parseDate(m.trim))


}

