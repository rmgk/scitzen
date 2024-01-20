package scitzen.project

import scitzen.compat.Logging
import scitzen.contexts.SastContext
import scitzen.outputs.AtomAnalyzer
import scitzen.sast.Fusion.Atoms
import scitzen.parser.Parse
import scitzen.sast.{Atom, Fusion, Sast, Section}

import scala.annotation.tailrec

class ArticleDirectory(globalFlags: Flags, val articles: Seq[Article]):
  val byPath: Map[ProjectPath, Seq[Article]] =
    articles.groupBy(fd => fd.doc.path)

  val labels: Map[String, Seq[SastRef]] =
    val all     = articles.map(_.context.labelledThings)
    val allKeys = all.iterator.flatMap(_.keysIterator).toSet
    allKeys.iterator.map { key =>
      key -> all.flatMap(_.getOrElse(key, Nil))
    }.toMap

  val titled: Seq[TitledArticle] = articles.flatMap(art =>
    art.titled.map(t =>
      TitledArticle(
        t,
        art,
        globalFlags.apply(t.attributes.plainList("flags"))
      )
    )
  )
  val fullArticles: Seq[TitledArticle] = titled.filter(_.header.prefix == "=")
  val subArticles: Seq[TitledArticle]  = titled.filterNot(_.header.prefix == "==")

  val byLabel: Map[String, TitledArticle]               = titled.iterator.map(t => Tuple2(t.header.autolabel, t)).toMap
  val byRef: Map[ArticleRef, TitledArticle]             = titled.iterator.map(a => Tuple2(a.article.ref, a)).toMap
  val snippetByRef: Map[ArticleRef, Article]            = articles.iterator.map(art => Tuple2(art.ref, art)).toMap
  def findByLabel(label: String): Option[TitledArticle] = byLabel.get(label)

  lazy val includes: Map[ArticleRef, Set[ArticleRef]] =
    articles.flatMap: article =>
      (article.ref, article.ref) +:
      article.context.includes.flatMap: directive =>
        References.resolve(directive, article.doc, this).map: target =>
          (article.ref, target.articleRef)
    .groupBy(_._1).view.mapValues(_.iterator.map(_._2).toSet).toMap

  lazy val includesFix: Map[ArticleRef, Set[ArticleRef]] =
    @tailrec
    def fixpointStep(current: Map[ArticleRef, Set[ArticleRef]]): Map[ArticleRef, Set[ArticleRef]] =
      val next = current.view.mapValues: includes =>
        includes.flatMap(current.get).flatten
      val combined = current.map: (k, v) =>
        (k, v ++ next.getOrElse(k, Set.empty))
      if combined == current then current
      else fixpointStep(combined)
    val start = System.nanoTime()
    val res   = fixpointStep(includes)
    Logging.cli.trace(s"fix took ${(System.nanoTime() - start) / 1000000}ms")
    res

object ArticleProcessing:

  def processArticles(doc: Document): List[Article] =
    items(doc).map: atoms =>
      val ref            = new ArticleRef(doc)
      val ctx            = new AtomAnalyzer(ref).convertSeq(atoms)(SastContext(()))
      val processedAtoms = ctx.data.toList
      val sast           = Fusion.fuseTop(processedAtoms, Nil)
      Article(ref, sast, doc, ctx.ret(()), processedAtoms)

  def headerType(atom: Atom) = atom match
    case Section(_, t @ ("=" | "=="), _, _) => t.length
    case _                                  => 3

  /** Splits the document into 3 parts:
    * • any initial text (not in any article)
    * • all sub articles (not in full articles)
    * • all full articles
    * Any sub articles within a full article are not returned on their own.
    */
  def items(document: Document): List[Atoms] =
    val atoms        = Parse.atoms(document)
    val (snip, rest) = atoms.span(s => headerType(s) == 3)
    @scala.annotation.tailrec
    def rec(rem: Atoms, acc: List[Atoms]): List[Atoms] =
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

  def loadDocuments(project: Project): Seq[Document] =
    Logging.cli.trace(s"discovering sources in ${project.root}")
    val sources = project.sources
    Logging.cli.trace(s"parsing ${sources.length} documents")
    sources.map: source =>
      Document(source)
