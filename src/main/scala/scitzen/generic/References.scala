package scitzen.generic

import scitzen.sast.{Block, Directive, Sast, Section}
import scitzen.compat.Logging.cli

import scala.jdk.CollectionConverters.*

case class SastRef(sast: Sast, articleRef: ArticleRef):
  def scope = articleRef.document.path

object References:

//  def containingDocument(articleRef: ArticleRef, directory: ArticleDirectory) =
//    val titled: List[TitledArticle] = directory.byRef(articleRef).article.context.includes.flatMap: inc =>
//      References.resolve(inc, doc, anal.directory).map: ref =>
//        anal.directory.byRef(ref.articleRef)
//    titled.map(_.article.ref).contains(targetDocument.articleRef)

  def resolve(directive: Directive, document: Document, directory: ArticleDirectory): Seq[SastRef] =
    val scope =
      directive.attributes.plain("scope").flatMap(document.resolve).getOrElse(document.path)
    References.filterCandidates(scope, directory.labels.getOrElse(directive.attributes.target, Nil))

  def filterCandidates(scope: ProjectPath, candidates: Seq[SastRef]): Seq[SastRef] =
    candidates match
      case Nil    => candidates
      case Seq(_) => candidates
      case multiple =>
        val searchScope = scope.absolute.iterator().asScala.toList
        val sorted = multiple.map { c =>
          Tuple2(
            c,
            c.scope.absolute.iterator().asScala.zip(searchScope).takeWhile {
              case (l, r) => l == r
            }.size
          )
        }.sortBy(_._2).reverse

        val best     = sorted.head._2
        val bestOnly = sorted.takeWhile(_._2 == best)
        (if bestOnly.size == 1 then bestOnly else sorted).map(_._1)

  def getLabel(targetRef: SastRef): Option[String] =
    targetRef.sast match
      case sec: Section => Some:
          if sec.level == -1 then "" else sec.ref
      case Block(_, attr, _) => attr.plain("unique ref")
      case other =>
        cli.warn(s"can not refer to $other")
        None
