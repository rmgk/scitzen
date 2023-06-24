package scitzen.bibliography

import de.rmgk.delay.{Async, Sync}
import scitzen.bibliography.BibManager.bibIds
import scitzen.compat.Logging
import scitzen.generic.{Project, ProjectPath}
import scitzen.sast.Attribute.Positional
import scitzen.sast.DCommand.{BibQuery, Cite}
import scitzen.sast.{Attribute, Attributes, Directive}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}

class BibManager(project: Project) {

  val dblpcachePath: ProjectPath = project.bibfileDBLPcache

  def parsebib(): Map[String, BibEntry] =
    (if Files.exists(dblpcachePath.absolute) then
       Bibtex.makeBib(dblpcachePath)
     else
       Map.empty
    ) ++
    project.bibfile.map(Bibtex.makeBib).getOrElse(Map.empty)

  def prefetch(citations: Set[Directive]): Async[Any, BibDB] = Sync:
    val currentBib = parsebib()

    val grouped           = citations.groupBy(_.command)
    val citeDirectives    = grouped.getOrElse(Cite, List.empty)
    val queriesDirectives = grouped.getOrElse(BibQuery, List.empty)

    val queries = queriesDirectives.map(cit => cit.attributes.target -> DBLP.search(cit.attributes.target)).toMap
    queries.foreach: (k, v) =>
      println(s"$k resolved to $v")

    val citeKeys     = citeDirectives.flatMap(BibManager.bibIds)
    val allCitations = citeKeys.toSet ++ queries.valuesIterator.flatten.map(info => s"DBLP:${info.key}")
    val missing      = allCitations -- currentBib.keySet
    val dblp         = missing.filter(_.startsWith("DBLP:"))
    if dblp.nonEmpty then
      Logging.scribe.info(s"scheduling download of ${dblp.size} missing citations")
      Files.createDirectories(dblpcachePath.directory)
      dblp.foreach: key =>
        DBLP.lookup(key.stripPrefix("DBLP:")).foreach: res =>
          Files.writeString(
            dblpcachePath.absolute,
            res,
            StandardCharsets.UTF_8,
            StandardOpenOption.APPEND,
            StandardOpenOption.CREATE
          )

    BibDB(parsebib(), queries)
}
object BibManager:
  extension (directive: Directive) {
    def bibIds: List[String] = directive.attributes.target.split(',').iterator.map(_.trim).toList
  }

case class BibDB(entries: Map[String, BibEntry], queried: Map[String, List[DBLPApi.Info]]):
  def convert(directive: Directive): Directive =
    val query = directive.attributes.target
    val keys  = queried.get(query).toList.flatten.map(info => s"DBLP:${info.key}")
    Directive(
      command = Cite,
      attributes = Attributes(directive.attributes.raw.map {
        case Positional(q) if q.plainString.trim == query.trim =>
          Attribute(keys.mkString(", "))
        case other => other
      } :+ Attribute("query", query))
    )(directive.prov)
  def bibkeys(directive: Directive): List[String] =
    directive.bibIds

object BibDB:
  def empty: BibDB = BibDB(Map.empty, Map.empty)
