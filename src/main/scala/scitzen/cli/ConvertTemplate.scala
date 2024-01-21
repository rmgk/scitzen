package scitzen.cli

import scitzen.bibliography.BibDB
import scitzen.blockconverters.BlockConversions
import scitzen.contexts.ConversionContext
import scitzen.project.{ArticleDirectory, Project, ProjectPath}
import scitzen.outputs.SastToTextConverter
import scitzen.sast.Attributes

object ConvertTemplate:
  def fillTemplate(
      project: Project,
      directory: ArticleDirectory,
      templatePath: Option[ProjectPath],
      templateSettings: Attributes
  ): ConversionContext[String] =
    templatePath match
      case None =>
        Logging.cli.warn(s"could not find template file")
        ConversionContext(templateSettings.plain("template content").getOrElse(""))
      case Some(templateFile) =>
        val templateArticle = directory.byPath(templateFile).head
        val documentString = new SastToTextConverter(
          ::(templateArticle.ref, Nil),
          ConversionAnalysis(
            project,
            directory,
            BlockConversions(Map.empty),
            BibDB.empty,
            None,
          ),
          templateSettings,
        ).convertSastSeq(ConversionContext(()), templateArticle.sast)
        documentString.map(_.mkString(""))
