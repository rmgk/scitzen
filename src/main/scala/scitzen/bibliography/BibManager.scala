package scitzen.bibliography

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, scanJsonValuesFromStream, writeToStream}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Async, Sync}
import scitzen.bibliography.BibManager.bibIds
import scitzen.compat.Logging
import scitzen.parser.{Biblet, Parse}
import scitzen.project.{Project, ProjectPath}
import scitzen.sast.DCommand.{BibQuery, Cite}
import scitzen.sast.{Attribute, Attributes, Directive}

import java.io.BufferedOutputStream
import java.nio.file.{Files, StandardOpenOption}
import scala.collection.mutable.ListBuffer
import scala.util.Using

given JsonValueCodec[BibEntry] = JsonCodecMaker.make

class BibManager(project: Project) {

  val dblpcachePath: ProjectPath = project.bibfileDBLPcache

  val biblets = project.bibfiles.flatMap: f =>
    Parse.bibfileUnwrap(Files.readAllBytes(f.absolute))
  val knowKeys = biblets.map(_.id).toSet
  if biblets.size != knowKeys.size then
    Logging.cli.warn("duplicate bib entries detected")
    biblets.groupBy(_.id).flatMap: (k, v) =>
      if v.sizeIs > 1 then Some(k)
      else None
    .foreach(k => println(s"\t$k"))

  val bibentriesCached =
    if !Files.exists(project.bibEntryCache.absolute) then Nil
    else
      val lb = new ListBuffer[BibEntry]
      Using(Files.newInputStream(project.bibEntryCache.absolute)): is =>
        scanJsonValuesFromStream[BibEntry](is): be =>
          lb.append(be)
          true
      .get
      lb.toList

  def prefetch(citations: Set[Directive]): Async[Any, BibDB] = Sync:

    val grouped           = citations.groupBy(_.command)
    val citeDirectives    = grouped.getOrElse(Cite, List.empty)
    val queriesDirectives = grouped.getOrElse(BibQuery, List.empty)

    val queries = queriesDirectives.map(cit => cit.attributes.target -> DBLP.search(cit.attributes.target)).toMap
    queries.foreach: (k, v) =>
      Logging.cli.info(s"$k resolved to $v")

    val citeKeys     = citeDirectives.flatMap(BibManager.bibIds)
    val allCitations = citeKeys.toSet ++ queries.valuesIterator.flatten.map(info => s"DBLP:${info.key}")
    val missing      = allCitations -- knowKeys
    val downloadedBiblets: List[Biblet] =
      if missing.isEmpty
      then Nil
      else
        Logging.cli.info(s"scheduling download of ${missing.size} missing citations")
        Files.createDirectories(dblpcachePath.absolute.getParent)
        missing.iterator.flatMap: uri =>
          List(DBLP, SemanticScholar).flatMap(_.lookup(uri))
            .iterator.tapEach: res =>
              Using(Files.newOutputStream(
                dblpcachePath.absolute,
                StandardOpenOption.APPEND,
                StandardOpenOption.CREATE
              )): os =>
                res.inputstream.transferTo(os)
        .toList

    val unknownBibentries = allCitations -- bibentriesCached.iterator.map(_.id).toSet

    val allBiblets: Seq[Biblet] = downloadedBiblets ++ biblets
    val bibletmap               = allBiblets.groupBy(_.id)

    val newBibentries: List[BibEntry] =
      if unknownBibentries.isEmpty
      then Nil
      else
        val all = unknownBibentries.iterator.flatMap(bibletmap.get).flatten.flatMap: biblet =>
          Bibtex.parse(biblet.inputstream)
        .toList
        Using(BufferedOutputStream(Files.newOutputStream(
          project.bibEntryCache.absolute,
          StandardOpenOption.APPEND,
          StandardOpenOption.CREATE
        ))): bo =>
          all.foreach: be =>
            writeToStream(be, bo)
            // writes a byte, even though the value is a char, and the method takes an int.
            bo.write('\n')
        .get
        all

    BibDB(Bibtex.makeBib(bibentriesCached ++ newBibentries), queries, bibletmap)
}

object BibManager:
  extension (directive: Directive) {
    def bibIds: List[String] = directive.attributes.target.split(',').iterator.map(_.trim).toList
  }

case class BibDB(
    entries: Map[String, BibEntry],
    queried: Map[String, List[DBLPApi.Info]],
    bibletmap: Map[String, Seq[Biblet]]
):
  def convertBibQuery(directive: Directive): Directive =
    val query = directive.attributes.target
    val keys  = queried.get(query).toList.flatten.map(info => s"DBLP:${info.key}")
    Directive(
      command = Cite,
      attributes = Attributes(directive.attributes.all.map {
        case Attribute("", raw, _) if raw.trim == query.trim =>
          Attribute(keys.mkString(", "))
        case other => other
      } :+ Attribute("query", query)),
      directive.meta
    )
  def bibkeys(directive: Directive): List[String] =
    directive.bibIds

object BibDB:
  def empty: BibDB = BibDB(Map.empty, Map.empty, Map.empty)
