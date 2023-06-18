package scitzen.contexts

import de.rmgk.Chain
import scitzen.bibliography.BibEntry
import scitzen.extern.Katex.{KatexConverter, KatexLibrary}
import scitzen.generic.ProjectPath
import scitzen.sast.Section

import java.nio.file.Path

/** The conversion context, used to keep state of in the conversion. */
case class ConversionContext[+T](
    data: T,
    katexConverter: KatexConverter = KatexConverter(Map.empty, new KatexLibrary(None)),
    resourceMap: Map[ProjectPath, Path] = Map.empty,
    usedCitations: List[BibEntry] = Nil,
    sections: List[Section] = Nil,
    features: Set[String] = Set.empty,
):
  def cite(citations: List[BibEntry]): ConversionContext[T] = copy(usedCitations = citations ::: usedCitations)

  def requireInOutput(source: ProjectPath, relative: Path): ConversionContext[T] =
    copy(resourceMap = resourceMap.updated(source, relative))

  def useFeature(name: String): ConversionContext[T] = copy(features = features.incl(name))

  def push(section: Section): ConversionContext[T] = copy(sections = section :: sections)

  def ret[U](d: U): ConversionContext[U]              = copy(data = d)
  def retc[U](d: U): ConversionContext[Chain[U]]      = copy(data = Chain(d))
  def map[U](f: T => U): ConversionContext[U]         = ret(f(data))
  def mapc[U](f: T => U): ConversionContext[Chain[U]] = ret(Chain(f(data)))

  def +:[I](value: I)(using T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => value +: data)
  def :+[I](value: I)(using T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => data :+ value)
  def ++:[I](value: Chain[I])(using T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => value ++ data)

  def :++[I](value: Chain[I])(using T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => data ++ value)

  def empty[U]: ConversionContext[Chain[U]]       = ret(Chain.empty[U])
  def single[U >: T]: ConversionContext[Chain[U]] = ret(Chain.one(data))

  def fold[U, V](seq: Seq[U])(f: (ConversionContext[Chain[V]], U) => ConversionContext[Chain[V]])
      : ConversionContext[Chain[V]] =
    seq.foldLeft(ret(Chain.empty[V])) { (ctx, elem) =>
      val nctx = f(ctx, elem)
      nctx.map(data => ctx.data ++ data)
    }

  def katex(key: String): ConversionContext[String] =
    val (res, kconv) = katexConverter.convert(key)
    kconv.fold(this)(kc => copy(katexConverter = kc)).ret(res)
