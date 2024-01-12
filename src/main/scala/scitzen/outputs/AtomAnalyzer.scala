package scitzen.outputs

import de.rmgk.Chain
import scitzen.contexts.SastContext
import scitzen.sast.Fusion.Atoms
import scitzen.project.{ArticleRef, SastRef}
import scitzen.sast.*
import scitzen.sast.DCommand.{BibQuery, Cite, Image, Include, Index, Ref}

class AtomAnalyzer(articleRef: ArticleRef):

  def document = articleRef.document

  type CtxCS   = SastContext[Chain[Container[Atom]]]
  type Ctx[+T] = SastContext[T]
  type Cta     = Ctx[?]

  def ensureUniqueRef(
      ctx: Cta,
      ref1: String,
      attr: Attributes,
  ): Ctx[(List[String], Attributes)] =
    val counter =
      if ctx.labelledThings.contains(ref1) then
        ctx.nextId.map(_.toString)
      else
        ctx.ret("")
    val newLabel = s"$ref1 (${document.uid}${counter.data})"
    val aliases  = ref1 :: newLabel :: attr.plain("aliases").toList.flatMap(_.split(',').toList)
    counter.ret((aliases, attr.updated("unique ref", newLabel)))

  def convertSeq(b: Atoms)(ctx: Cta): CtxCS =
    ctx.fold(b) { (ctx, sast) => convertSingle(sast)(ctx).single }

  def convertSingle(container: Container[Atom])(ctx: Cta): Ctx[Container[Atom]] =
    val newContent: Ctx[Atom] = container.content match
      case sc: SpaceComment => ctx.ret(sc)

      case text: Text =>
        convertInlines(ctx, text.inl).map(i => Text(i.toSeq))

      case fenced: Fenced =>
        val refctx = ensureRefForLabel(fenced, ctx)
        if refctx.data.command == BCommand.Convert
        then refctx.addConversionBlock(refctx.data)
        else refctx

      case delimiter: Delimiter => ensureRefForLabel(delimiter, ctx)

      case sec @ Section(title, level, _) =>
        val ctxWithRef = ensureRefForLabel(sec, ctx)
        val newSection = ctxWithRef.data
        val conCtx     = ctxWithRef.addSection(newSection)
        convertInlines(conCtx, title.inl).map { title =>
          Section(Text(title.toList), level, newSection.attributes)
        }

      case ListAtom(m, cont)           => convertInlines(ctx, cont).map(i => ListAtom(m, cont))
      case DefinitionListAtom(m, cont) => convertInlines(ctx, cont).map(i => DefinitionListAtom(m, cont))

      case mcro: Directive => convertDirective(mcro)(ctx)

    newContent.map(c => container.copy(content = c))

  trait Labellable[T]:
    extension (t: T)
      def label: Option[String]
      def attributes: Attributes
      def withAttributes(attributes: Attributes): T
  object Labellable:
    given Labellable[Section] with {
      extension (t: Section)
        override def label: Option[String]                           = Some(t.autolabel)
        override def attributes: Attributes                          = t.attributes
        override def withAttributes(attributes: Attributes): Section = t.copy(attributes = attributes)
    }
    given Labellable[Fenced] with {
      extension (t: Fenced)
        override def label: Option[String]                          = t.attributes.plain("label")
        override def attributes: Attributes                         = t.attributes
        override def withAttributes(attributes: Attributes): Fenced = t.copy(attributes = attributes)
    }
    given Labellable[Delimiter] with {
      extension (t: Delimiter)
        override def label: Option[String]                             = t.attributes.plain("label")
        override def attributes: Attributes                            = t.attributes
        override def withAttributes(attributes: Attributes): Delimiter = t.copy(attributes = attributes)
    }

  private def ensureRefForLabel[T <: Atom: Labellable](entity: T, ctx: Cta): Ctx[T] = {
    entity.label match
      case None => ctx.ret(entity)
      case Some(ref) =>
        val resctx          = ensureUniqueRef(ctx, ref, entity.attributes)
        val (aliases, attr) = resctx.data
        val ublock          = entity.withAttributes(attr)
        val target          = SastRef(ublock, articleRef)
        refAliases(resctx, aliases, target).ret(ublock)
  }

  private def refAliases(resctx: Ctx[?], aliases: List[String], target: SastRef): Ctx[Unit] =
    aliases.foldLeft(resctx.ret(()))((c: Ctx[?], a) => c.addRefTarget(a, target).ret(()))

  def convertInlines(ctx: Cta, inners: Seq[Inline]): Ctx[Chain[Inline]] =
    ctx.fold(inners) { (ctx, inline) =>
      inline match
        case inlineText: InlineText => ctx.ret(Chain.one(inlineText))
        case m: Directive           => convertDirective(m)(ctx).single
    }

  def convertDirective(directive: Directive)(ctx: Cta): Ctx[Directive] =
    directive.command match
      case Image           => ctx.addImage(directive).ret(directive)
      case Include         => ctx.addInclude(directive).ret(directive)
      case Ref | Index     => ctx.addReference(directive).ret(directive)
      case Cite | BibQuery =>
        // TODO this is a temporary way to rename the parameter, remove at some point
        val res =
          val style = directive.attributes.plain("style")
          if style.contains("name") then
            directive.copy(attributes = directive.attributes.updated("style", "author"))(directive.prov)
          else
            directive
        ctx.addCitation(res).ret(res)
      case _ => ctx.ret(directive)
