package scitzen.contexts

import better.files.File
import cats.data.Chain
import scitzen.extern.ConvertTask
import scitzen.generic.SastRef
import scitzen.sast.{Macro, Section}

import java.nio.file.Path

/** The conversion context, used to keep state of in the conversion. */
case class SastContext[T](
    data: T,
    resourceMap: Map[File, Path] = Map.empty,
    tasks: List[ConvertTask] = Nil,
    labelledThings: Map[String, List[SastRef]] = Map.empty,
    uniquectr: Int = 0,
    includes: List[File] = Nil,
    partialMacros: List[Macro] = Nil,
    sections: List[Section] = Nil,
) {
  def requireInOutput(source: File, relative: Path): SastContext[T] = {
    copy(resourceMap = resourceMap.updated(source, relative))
  }

  def addMacro(mcro: Macro): SastContext[T] = copy(partialMacros = mcro :: partialMacros)

  def nextId: SastContext[Int] = copy(uniquectr = uniquectr + 1, data = uniquectr)

  def addRefTarget(ref: String, secref: SastRef): SastContext[SastRef] = {
    val old = labelledThings.getOrElse(ref, Nil)
    copy(labelledThings = labelledThings.updated(ref, secref :: old), data = secref)
  }

  def push(section: Section): SastContext[T] = copy(sections = section :: sections)

  def ret[U](d: U): SastContext[U]      = copy(data = d)
  def map[U](f: T => U): SastContext[U] = ret(f(data))

  def single[U >: T]: SastContext[Chain[U]] = ret(Chain.one(data))

  def fold[U, V](seq: Seq[U])(f: (SastContext[Chain[V]], U) => SastContext[Chain[V]]): SastContext[Chain[V]] =
    seq.foldLeft(ret(Chain.empty[V])) { (ctx, elem) =>
      val nctx = f(ctx, elem)
      nctx.map(data => ctx.data ++ data)
    }

  def execTasks(): SastContext[T] = {
    import scala.jdk.CollectionConverters._
    tasks.asJava.parallelStream().forEach { ct => ct.run() }
    copy(tasks = Nil)
  }

}
