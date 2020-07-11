package scitzen.generic

import better.files.File
import scitzen.parser.Sast.{Macro, Section}
import scitzen.parser.{DateParsingHelper, MacroCommand, Sast, ScitzenDateTime}

case class Article(header: Section, content: List[Sast], sourceDoc: Document, includes: DocumentDirectory) {

  lazy val language: Option[String] = header.attributes.named.get("language").map(_.trim)

  lazy val date: Option[ScitzenDateTime] = header.attributes.named.get("date")
    .map(v => DateParsingHelper.parseDate(v.trim))

  lazy val title: String = header.title.str

  lazy val named: Map[String, String] = header.attributes.named

  lazy val analyzed: AnalyzeResult = {
    new SastAnalyzer(sourceDoc.reporter).analyze(content)
  }
}

object Article {
  def notArticleHeader(sast: Sast): Boolean =
    sast match {
      case Section(title, "=", attributes) => false
      case other                           => true
    }

  def recursiveIncludes(article: Article, project: Project, documentDirectory: DocumentDirectory): DocumentDirectory = {

    @scala.annotation.tailrec
    def rec(toCheck: List[File], knownIncludes: List[File]): List[File] = {
      val newKnown = toCheck reverse_::: knownIncludes
      val newIncludes =
        toCheck.iterator
          .flatMap(documentDirectory.byPath.get)
          .flatMap(_.includes)
          .filterNot(f => newKnown.contains(f)).toList
      if (newIncludes.isEmpty) newKnown
      else rec(newIncludes, newKnown)
    }

    val initialIncludes: List[File] =
      new SastAnalyzer(article.sourceDoc.reporter).analyze(article.content).macros.collect {
        case Macro(MacroCommand.Include, attributes) =>
          project.resolve(article.sourceDoc.file.parent, attributes.target)
      }.flatten

    val includes = rec(initialIncludes, Nil)
    val incd  = documentDirectory.documents.filter(d => includes.contains(d.file))
    DocumentDirectory(incd)

  }

  def articles(document: Document): List[Article] = {
    @scala.annotation.tailrec
    def rec(rem: List[Sast], acc: List[Article]): List[Article] = {
      rem.dropWhile(notArticleHeader) match {
        case (sec @ Section(title, "=", attributes)) :: rest =>
          val (cont, other) = rest.span(notArticleHeader)
          rec(other, Article(sec, cont, document, DocumentDirectory(Nil)) :: acc)
        case other => acc
      }
    }
    rec(document.sast, Nil)
  }
}
