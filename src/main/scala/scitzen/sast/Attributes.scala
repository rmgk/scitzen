package scitzen.sast

import scala.collection.View

case class Attributes(raw: Seq[Attribute]) {

  def positional: View[Attribute] = raw.view.filter:
    case Attribute(id, raw, text) => id.isEmpty

  def target: String = positional.lastOption.fold("")(_.asTarget)
  def text: Text     = positional.headOption.fold(Text.empty)(_.text)
  def textOption: Option[Text] =
    if positional.sizeIs > 1 then
      Some(text)
    else None

  def get(id: String): Option[Attribute] = raw.findLast(_.id == id)
  def plain(id: String): Option[String]  = get(id).map(_.asTarget)

  def plainList(id: String) =
    def splitlist(attribute: Attribute) = attribute.text.plainString.split(',').map(_.trim).filter(_.nonEmpty)
    raw.iterator.filter(_.id == id).flatMap(splitlist)

  private def append(other: Seq[Attribute]): Attributes = Attributes(raw ++ other)

  private def remove(key: String): Attributes = Attributes(raw.filterNot(_.id == key))
  def updated(key: String, value: String) = {
    remove(key).append(List(Attribute.apply(key, value)))
  }
}

object Attributes {
  def target(string: String): Attributes = Attributes(List(Attribute("", string)))
  val empty                              = Attributes(Nil)
}

case class Attribute(id: String, raw: String, text: Text):
  def asTarget: String = raw.strip()

object Attribute {
  def apply(value: String): Attribute             = apply("", value)
  def apply(id: String, value: String): Attribute = apply(id, value, Text.of(value))
}
