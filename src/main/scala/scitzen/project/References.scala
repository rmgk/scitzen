package scitzen.project

import scitzen.cli.Logging.cli
import scitzen.sast.{Atom, Block, Delimiter, Directive, Fenced, Sast, Section}

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

case class SastRef(atom: Atom, articleRef: ArticleRef):
  def scope = articleRef.document.path

object References:
  def resolve(directive: Directive, document: Document, directory: ArticleDirectory): Seq[SastRef] =
    val scope =
      directive.attributes.plain("scope").flatMap(document.resolve).getOrElse(document.path)
    val candidates =
      val target = directive.attributes.target
      val byPath = document.resolve(target).flatMap(directory.byPath.get).flatMap(_.headOption).map: (art: Article) =>
        SastRef(art.atoms.head, art.ref)
      .toList
      val byLabel = directory.labels.getOrElse(directive.attributes.target, Nil)
      byPath ++ byLabel
    References.filterCandidates(scope, candidates, _.scope.absolute)

  def resolveResource(project: Project, doc: Document, targetString: String): Seq[ProjectPath] =
    if targetString.matches(raw"^\.*[/\\].*")
    then doc.resolve(targetString).toList
    else
      val targetPath = Path.of(targetString)
      val candidates =
        project.projectFilenames.getOrElse(targetPath.getFileName, Nil).filter(_.absolute.endsWith(targetPath))
      filterCandidates(doc.path, candidates, _.absolute)

  def filterCandidates[T](scope: ProjectPath, candidates: Seq[T], by: T => Path): Seq[T] =
    candidates match
      case Nil    => candidates
      case Seq(_) => candidates
      case multiple =>
        val searchScope = scope.absolute.iterator().asScala.toList
        val sorted = multiple.map { c =>
          Tuple2(
            c,
            by(c).iterator().asScala.zip(searchScope).takeWhile {
              case (l, r) => l == r
            }.size
          )
        }.sortBy(_._2).reverse

        val best     = sorted.head._2
        val bestOnly = sorted.takeWhile(_._2 == best)
        (if bestOnly.size == 1 then bestOnly else sorted).map(_._1)

  def getLabel(targetRef: SastRef): Option[String] =
    targetRef.atom match
      case sec: Section => Some:
          if sec.level == -1 then "" else sec.ref
      case block: Fenced    => block.attributes.plain("unique ref")
      case block: Delimiter => block.attributes.plain("unique ref")
      case other =>
        cli.warn(s"can not refer to $other")
        None
