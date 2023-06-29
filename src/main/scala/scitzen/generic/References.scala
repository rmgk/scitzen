package scitzen.generic

import scitzen.sast.{Block, Sast, Section}
import scitzen.compat.Logging.cli

import scala.jdk.CollectionConverters.*

case class SastRef(scope: ProjectPath, sast: Sast, directArticle: Option[Section])

object References:

  def filterCandidates(scope: ProjectPath, candidates: List[SastRef]): List[SastRef] =
    candidates match
      case Nil     => candidates
      case List(_) => candidates
      case multiple =>
        val searchScope = scope.absolute.iterator().asScala.toList
        val sorted = multiple.map { c =>
          c ->
          c.scope.relativeToProject.iterator().asScala.toList.zip(searchScope).takeWhile {
            case (l, r) => l == r
          }.size
        }.sortBy(_._2).reverse

        val best     = sorted.head._2
        val bestOnly = sorted.takeWhile(_._2 == best)
        (if bestOnly.size == 1 then bestOnly else sorted).map(_._1)

  def getLabel(targetRef: SastRef): Option[String] =
    targetRef.sast match
      case sec: Section      => Some(sec.ref)
      case Block(_, attr, _) => attr.plain("unique ref")
      case other =>
        cli.warn(s"can not refer to $other")
        None
