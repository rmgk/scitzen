package scitzen.blockconverters

import scitzen.sast.{Attributes, BCommand, Block, Fenced, Sast}

import java.nio.file.Files

object FileContentModule extends BlockConverterModule {

  override def handles: String = "load"
  def convert(converterParams: ConverterParams): List[Sast] = {
    import converterParams.*
    article.doc.resolve(attributes.target) match
      case Some(path) =>
        List(Block(BCommand.Empty, Attributes.empty, Fenced(Files.readString(path.absolute)))(block.prov))
      case None => Nil
  }
}
