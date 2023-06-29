package scitzen.generic

import scitzen.compat.Logging
import scitzen.contexts.SastContext
import scitzen.outputs.SastToSastConverter
import scitzen.parser.Parse
import scitzen.sast.{Sast, Section}

import java.nio.file.{Files, Path}
import scala.util.Using

class ArticleDirectory(val articles: List[Article]):
  val byPath: Map[ProjectPath, List[Article]] =
    articles.groupBy(fd => fd.doc.path)

  val labels: Map[String, List[SastRef]] =
    val all     = articles.map(_.context.labelledThings)
    val allKeys = all.iterator.flatMap(_.keysIterator).toSet
    allKeys.iterator.map { key =>
      key -> all.flatMap(_.getOrElse(key, Nil))
    }.toMap

  val titled: List[TitledArticle]       = articles.flatMap(art => art.titled.map(t => TitledArticle(t, art)))
  val fullArticles: List[TitledArticle] = titled.filter(_.header.prefix == "=")
  val subArticles: List[TitledArticle]  = titled.filterNot(_.header.prefix == "==")

  val byLabel: Map[String, TitledArticle]               = titled.iterator.map(t => Tuple2(t.header.autolabel, t)).toMap
  val byRef: Map[ArticleRef, TitledArticle]             = titled.iterator.map(a => Tuple2(a.article.ref, a)).toMap
  def findByLabel(label: String): Option[TitledArticle] = byLabel.get(label)

object ArticleProcessing:

  def processArticles(doc: Document, project: Project): List[Article] =
    items(doc).map: art =>
      val ref = new ArticleRef(doc)
      val ctx = new SastToSastConverter(doc, ref).convertSeq(art)(SastContext(()))
      Article(ref, ctx.data.toList, doc, ctx.ret(()), art)

  def headerType(sast: Sast) = sast match
    case Section(_, t @ ("=" | "=="), _) => t.length
    case _                               => 3

  /** Splits the document into 3 parts:
    * • any initial text (not in any article)
    * • all sub articles (not in full articles)
    * • all full articles
    * Any sub articles within a full article are not returned on their own.
    */
  def items(document: Document): List[List[Sast]] =
    val sast         = Parse.documentUnwrap(document)
    val (snip, rest) = sast.span(s => headerType(s) == 3)
    @scala.annotation.tailrec
    def rec(rem: List[Sast], acc: List[List[Sast]]): List[List[Sast]] =
      rem match
        case Nil => acc.reverse
        case (h: Section) :: t =>
          val htype        = headerType(h)
          val (body, rest) = t.span(s => headerType(s) > htype)
          rec(rest, (h :: body) :: acc)
        case other :: rest =>
          throw new IllegalStateException(s"unexpected sast when looking for item: $other")
    val res = rec(rest, Nil)
    if snip.isEmpty
    then res
    else snip :: res

  def isScim(c: Path): Boolean =
    Files.isRegularFile(c) &&
    c.getFileName.toString.endsWith(".scim")

  /** returs a list of .scim files starting at `source`. Does not follow symlinks and ignores files and folders starting with a . */
  def discoverSources(source: Path): List[Path] =
    import scala.jdk.CollectionConverters.*
    def hasDotComponent(c: Path): Boolean =
      source.relativize(c).iterator().asScala.exists { _.toString.startsWith(".") }

    Using(Files.walk(source)) { stream =>
      stream.iterator().asScala.filter { c =>
        isScim(c) && !hasDotComponent(c)
      }.toList
    }.get

  def loadDocuments(project: Project): List[Document] =
    Logging.cli.trace(s"discovering sources in ${project.root}")
    val sources: List[Path] = discoverSources(project.root)
    Logging.cli.trace(s"parsing ${sources.length} documents")
    sources.map: source =>
      Document(project.asProjectPath(source))
