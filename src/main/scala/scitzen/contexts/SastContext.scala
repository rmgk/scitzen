package scitzen.contexts

import de.rmgk.Chain
import scitzen.project.SastRef
import scitzen.sast.{Block, Directive, Fenced, FusedDelimited, Section}

/** The conversion context, used to keep state of the conversion. */
case class SastContext[+T](
    data: T,
    labelledThings: Map[String, List[SastRef]] = Map.empty,
    uniquectr: Int = 0,
    imageDirectives: List[Directive] = Nil,
    fencedConvert: List[Fenced] = Nil,
    delimitedConvert: List[FusedDelimited] = Nil,
    sections: List[Section] = Nil,
    citations: List[Directive] = Nil,
    references: List[Directive] = Nil,
    includes: List[Directive] = Nil,
):
  def addImage(image: Directive): SastContext[T]        = copy(imageDirectives = image :: imageDirectives)
  def addConversionBlock(block: Fenced): SastContext[T] = copy(fencedConvert = block :: fencedConvert)
  def addSection(section: Section): SastContext[T]      = copy(sections = section :: sections)
  def addCitation(cite: Directive): SastContext[T]      = copy(citations = cite :: citations)
  def addReference(ref: Directive): SastContext[T]      = copy(citations = ref :: citations)
  def addInclude(include: Directive): SastContext[T]    = copy(includes = include :: includes)

  def nextId: SastContext[Int] = copy(uniquectr = uniquectr + 1, data = uniquectr)

  def addRefTarget(ref: String, secref: SastRef): SastContext[SastRef] =
    val old = labelledThings.getOrElse(ref, Nil)
    copy(labelledThings = labelledThings.updated(ref, secref :: old), data = secref)

  def ret[U](d: U): SastContext[U]      = copy(data = d)
  def map[U](f: T => U): SastContext[U] = ret(f(data))

  def single[U >: T]: SastContext[Chain[U]] = ret(Chain.one(data))

  def fold[U, V](seq: Seq[U])(f: (SastContext[Chain[V]], U) => SastContext[Chain[V]]): SastContext[Chain[V]] =
    seq.foldLeft(ret(Chain.empty[V])) { (ctx, elem) =>
      val nctx = f(ctx, elem)
      nctx.map(data => ctx.data ++ data)
    }
