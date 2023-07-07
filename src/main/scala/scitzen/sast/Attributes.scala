package scitzen.sast

import de.rmgk.logging.Logger
import scitzen.compat.Logging
import scitzen.sast.Attribute.Positional
import scitzen.sast.Attributes.{Flags, splitlist}

import scala.collection.View

case class Attributes(raw: Seq[Attribute]) {

  def positional: View[Positional] = raw.view.collect:
    case p: Positional => p

  def target: String = positional.lastOption.fold("")(_.text.plainString)
  def text: Text     = positional.headOption.fold(Text.empty)(_.text)
  def textOption: Option[Text] =
    if positional.sizeIs > 1 then
      Some(text)
    else None

  def get(id: String): Option[Attribute] = raw.findLast(_.id == id)
  def plain(id: String): Option[String]  = get(id).map(_.text.plainString)

  def plainList(id: String) = raw.iterator.filter(_.id == id).flatMap(splitlist)

  lazy val flags: Flags =
    plainList("flags").foldLeft(Flags.default): (curr, flag) =>
      curr.update(flag)

  private def append(other: Seq[Attribute]): Attributes = Attributes(raw ++ other)

  private def remove(key: String): Attributes = Attributes(raw.filterNot(_.id == key))
  def updated(key: String, value: String) = {
    remove(key).append(List(Attribute.apply(key, value)))
  }
}

object Attributes {

  def splitlist(attribute: Attribute) = attribute.text.plainString.split(',').map(_.trim).filter(_.nonEmpty)

  case class Flags(
      html: Boolean,
      tex: Boolean,
      hardwrap: Boolean,
      justify: Boolean,
      `section numbers`: Boolean,
      hidden: Boolean
  ):
    def update: String => Flags =
      case "+html"            => copy(html = true)
      case "-html"            => copy(html = false)
      case "+tex"             => copy(tex = true)
      case "-tex"             => copy(tex = false)
      case "+justify"         => copy(justify = true)
      case "-justify"         => copy(justify = false)
      case "+hardwrap"        => copy(justify = true)
      case "-hardwrap"        => copy(justify = false)
      case "+section numbers" => copy(`section numbers` = true)
      case "-section numbers" => copy(`section numbers` = false)
      case "+hidden"          => copy(hidden = true)
      case "-hidden"          => copy(hidden = false)
      case other: String =>
        Logging.cli.warn(s"unknown flag", other)
        this
  object Flags:
    def default: Flags =
      Flags(html = true, tex = false, hardwrap = false, justify = true, `section numbers` = true, hidden = false)

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
