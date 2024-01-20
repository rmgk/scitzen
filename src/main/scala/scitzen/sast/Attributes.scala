package scitzen.sast

import scala.collection.View

/** Attributes handle access to a sequence of Attribute values.
  * For the most part, directives have zero, one, or two positional attributes, and these have shorthand accessors as follows.
  *
  * If we have an attribute { " :{a} "; " :{b} "; " :{c} "} then:
  *
  * • target: ":{c}"
  * • specifier: Some(":{a}")
  * • text: resolved a
  * • description: Some(resolved a)
  *
  * For a singular attribute { " :{a} "}:
  *
  * • target: ":{a}"
  * • specifier: None
  * • text: resolved a
  * • description: None
  *
  * For an empty attribute {}:
  *
  * • target: ""
  * • specifier: None
  * • text: Text.empty
  * • description: None
  */
case class Attributes(all: Seq[Attribute]) {

  def positional: View[Attribute] = all.view.filter:
    case Attribute(id, raw, text) => id.isEmpty

  def target: String = positional.lastOption.fold("")(_.asTarget)

  def specifier: Option[String] =
    if positional.sizeIs > 1 then
      positional.headOption.map(_.asTarget)
    else None

  def text: Text = positional.headOption.fold(Text.empty)(_.text)

  def description: Option[Text] =
    if positional.sizeIs > 1 then
      Some(text)
    else None

  def get(id: String): Option[Attribute] = all.findLast(_.id == id)
  def plain(id: String): Option[String]  = get(id).map(_.asTarget)

  def plainList(id: String) =
    def splitlist(attribute: Attribute) = attribute.raw.split(',').map(_.strip()).filter(_.nonEmpty)
    all.iterator.filter(_.id == id).flatMap(splitlist)

  private def append(other: Seq[Attribute]): Attributes = Attributes(all ++ other)

  private def remove(key: String): Attributes = Attributes(all.filterNot(_.id == key))
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
