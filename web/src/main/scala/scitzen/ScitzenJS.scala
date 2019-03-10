package scitzen

import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.html.TextArea
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.{article, main => tagmain}
import scitzen.converter.HtmlConverter
import scitzen.parser.Adoc

object ScitzenJS {

  def parseAndStuff(ev: Event): Boolean = {
    val target = ev.currentTarget.asInstanceOf[TextArea]
    val value = target.value
    println(s"current vlue is $value")
    val doc = Adoc.document(value).get

    if (doc.blocks.isEmpty) return false

    val res = new HtmlConverter(JsDom).convert(doc)
    val parent = target.parentElement
    val rendered = article(cls := "fullpost", res).render
    def resetHandler(ev: Event) = {
      parent.replaceChild(target, rendered)
      target.focus()
      false
    }
    rendered.addEventListener("dblclick", resetHandler)
    parent.replaceChild(rendered, target)
    false
  }

  def resizetoContent(ev: Event) = {
    val target = ev.currentTarget.asInstanceOf[TextArea]
    target.style.height = ""
    target.style.height = target.scrollHeight + "px"
  }

  def main(args: Array[String]): Unit = {
    println("hello world")
    dom.document.body = body(tagmain(textarea(onblur := parseAndStuff _,
                                              oninput := resizetoContent _))).render
  }
}


