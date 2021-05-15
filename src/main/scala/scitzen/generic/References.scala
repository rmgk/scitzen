package scitzen.generic

import better.files.File
import scitzen.sast.{Block, Sast, Section}

import scala.jdk.CollectionConverters.*

case class SastRef(scope: File, sast: Sast, directArticle: Option[Article])

object References:

  def filterCandidates(scope: File, candidates: List[SastRef]): List[SastRef] =
    candidates match
      case Nil     => candidates
      case List(_) => candidates
      case multiple =>
        val searchScope = scope.path.iterator().asScala.toList
        val sorted = multiple.map { c =>
          c ->
            c.scope.path.iterator().asScala.toList.zip(searchScope).takeWhile {
              case (l, r) => l == r
            }.size
        }.sortBy(_._2).reverse

        val best     = sorted.head._2
        val bestOnly = sorted.takeWhile(_._2 == best)
        (if bestOnly.size == 1 then bestOnly else sorted).map(_._1)

  def getLabel(targetDocument: SastRef): Option[String] =
    targetDocument.sast match
      case sec: Section      => Some(sec.ref)
      case Block(attr, _, _) => attr.named.get("label")
      case other =>
        scribe.error(s"can not refer to $other")
        None
