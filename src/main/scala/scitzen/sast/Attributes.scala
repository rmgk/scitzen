package scitzen.sast

import scitzen.sast.Attribute.Positional

import scala.collection.View

case class Attributes(raw: Seq[Attribute]) {

  def positional: View[Positional] = raw.view.collect:
    case p: Positional => p

  def target: String = positional.lastOption.fold("")(_.text.plainString)
  def text: Text     = positional.headOption.fold(Text.empty)(_.text)
  def textOption: Option[Text] =
    val pos = positional
    if pos.sizeIs > 1 then
      Some(text)
    else None

  def get(id: String): Option[Attribute] = raw.findLast(_.id == id)
  def plain(id: String): Option[String]  = get(id).map(_.text.plainString)

  private def append(other: Seq[Attribute]): Attributes = Attributes(raw ++ other)

  private def remove(key: String): Attributes = Attributes(raw.filterNot(_.id == key))
  def updated(key: String, value: String) = {
    remove(key).append(List(Attribute.apply(key, value)))
  }
}

object Attributes {
  def target(string: String): Attributes = Attribute("", string).toAttributes
  val empty                              = Attributes(Nil)
}

sealed trait Attribute {
  def toAttributes: Attributes = Attributes(List(this))
  def id: String
  def text: Text
}

object Attribute {
  def apply(value: String): Positional = apply(Text.of(value))
  def apply(value: Text): Positional   = Positional(value)
  def apply(id: String, value: String): Attribute =
    apply(id, Text.of(value))
  def apply(id: String, value: Text): Attribute =
    if id == ""
    then Positional(value)
    else Named(id, value)

  case class Positional(text: Text) extends Attribute:
    def id: String = ""
  case class Named(id: String, text: Text) extends Attribute
  case class Nested(id: String, inner: Attributes) extends Attribute {
    def text: Text = inner.text
  }

}
