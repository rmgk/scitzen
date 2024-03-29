package scitzen.blockconverters

import scitzen.sast.{Attributes, BCommand, Block, Fenced, Prov, Sast}

import java.nio.file.Files

object FileContentModule extends BlockConverterModule {

  override def handles: String = "load"
  def convert(converterParams: ConverterParams): List[Sast] = {
    import converterParams.*
    article.doc.resolve(attribute.asTarget) match
      case Some(path) =>
        List(Fenced(BCommand.Empty, Attributes.empty, Files.readString(path.absolute), converterParams.block.meta))
      case None => Nil
  }
}
