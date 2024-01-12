package scitzen.blockconverters

import scitzen.contexts.ConversionContext
import scitzen.project.{Article, ArticleDirectory, Project}
import scitzen.sast.{Attributes, Block, Fenced}

case class ConverterParams(
    project: Project,
    articleDirectory: ArticleDirectory,
    article: Article,
    block: Fenced,
    content: String,
    attributes: Attributes,
    resctx: Option[ConversionContext[?]]
)
