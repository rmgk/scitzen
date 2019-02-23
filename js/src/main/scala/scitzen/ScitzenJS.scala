package scitzen

import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.html.{Element, TextArea}
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.{article, main => tagmain}
import scitzen.converter.HtmlConverter
import scitzen.parser.Adoc

object ScitzenJS {

  def parseAndStuff(container: Element)(ev: Event): Boolean = {
    val value = ev.currentTarget.asInstanceOf[TextArea].value
    println(s"current vlue is $value")
    val doc = Adoc.document(value).get
    val res = new HtmlConverter(JsDom).convert(doc)
    container.innerHTML = ""
    container.appendChild(res.render)
    false
  }

  def main(args: Array[String]): Unit = {
    println("hello world")
    val container = article(cls := "fullpost", contenteditable := "true").render
    dom.document.body = body(textarea(oninput := parseAndStuff(container) _), tagmain(container)).render
  }
}
