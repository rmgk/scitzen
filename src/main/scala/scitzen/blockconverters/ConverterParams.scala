package scitzen.blockconverters

import scitzen.contexts.ConversionContext
import scitzen.project.{Article, ArticleDirectory, Project}
import scitzen.sast.{Attributes, Block}

case class ConverterParams(
    project: Project,
    articleDirectory: ArticleDirectory,
    article: Article,
    block: Block,
    content: String,
    attributes: Attributes,
    resctx: Option[ConversionContext[?]]
)
