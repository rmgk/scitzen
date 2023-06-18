package scitzen.contexts

import de.rmgk.Chain
import scitzen.generic.SastRef
import scitzen.sast.{Block, Directive, Section}

/** The conversion context, used to keep state of the conversion. */
case class SastContext[+T](
    data: T,
    labelledThings: Map[String, List[SastRef]] = Map.empty,
    uniquectr: Int = 0,
    imageDirectives: List[Directive] = Nil,
    convertBlocks: List[Block] = Nil,
    sections: List[Section] = Nil,
    citations: List[Directive] = Nil
):
  def addImage(image: Directive): SastContext[T]       = copy(imageDirectives = image :: imageDirectives)
  def addConversionBlock(block: Block): SastContext[T] = copy(convertBlocks = block :: convertBlocks)
  def addSection(section: Section): SastContext[T]     = copy(sections = section :: sections)
  def addCitation(cite: Directive): SastContext[T]     = copy(citations = cite :: citations)

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
