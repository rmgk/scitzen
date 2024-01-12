package scitzen.compat

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import scitzen.sast.*

object MirrorToSast {

  import scitzen.compat.CodeMirror as C

  def convert(document: C.Document) = {
    val blocks = document.doc.asInstanceOf[C.doc].content
    blocks.map(convertBlock)
  }

  def convertBlock(block: C.Block): Sast = block match {
    case CodeMirror.heading(attrs, content) =>
      Section(Text(content.map(convertInline)), "#" * (attrs.level - 1), Attributes(Nil))
    case CodeMirror.paragraph(content) =>
      println(s"converting $content")
      val res = Paragraph(Seq(Container("", Text(content.map(convertInline)), Prov())))
      println(s"to $res")
      res
  }

  def convertInline(value: C.Inline): Inline = value match {
    case CodeMirror.text(text, marks) => InlineText(text)
    case CodeMirror.image(attrs)      => Directive(DCommand.Image, Attributes.target(attrs.src))(Prov())
  }
}

object CodeMirror {

  case class Document(doc: DocAlternatives, selection: Selection)

  case class Selection(`type`: String, anchor: Int, head: Int)

  sealed trait DocAlternatives
  case class doc(content: List[Block]) extends DocAlternatives

  sealed trait Inline
  case class text(text: String, marks: List[Mark]) extends Inline
  case class image(attrs: ImageAttrs)              extends Inline

  case class ImageAttrs(src: String, alt: Option[String], title: Option[String])

  sealed trait Mark
  case object em                    extends Mark
  case object strong                extends Mark
  case class link(attrs: LinkAttrs) extends Mark
  case object code                  extends Mark

  case class Unknown()

  case class HeadingAttr(level: Int)
  case class LinkAttrs(href: String, title: Option[String])

  sealed trait Block
  case class heading(attrs: HeadingAttr, content: List[Inline]) extends Block
  case class paragraph(content: List[Inline])                   extends Block

  implicit val documentRW: JsonValueCodec[Document] =
    JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

}
