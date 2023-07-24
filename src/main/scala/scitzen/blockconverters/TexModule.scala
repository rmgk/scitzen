package scitzen.blockconverters

import scitzen.extern.{Hashes, Latexmk}
import scitzen.sast.{Attribute, Attributes, DCommand, Directive, Sast}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object TexModule extends BlockConverterModule {

  override def handles: String = "tex"

  override def convert(converterParams: ConverterParams): List[Sast] =
    import converterParams.*
    val texbytes    = content.getBytes(StandardCharsets.UTF_8)
    val contentHash = Hashes.sha1hex(texbytes)
    val target      = project.cachePath(Path.of(s"subtex/$contentHash/$contentHash.pdf"))
    val res =
      if Files.exists(target.absolute)
      then Some(target.absolute)
      else
        val dir = target.absolute.getParent
        Files.createDirectories(dir)
        val texfile = dir.resolve(contentHash + ".tex")
        Files.write(texfile, texbytes)
        resctx.foreach: ctx =>
          ctx.fileDependencies.foreach: dep =>
            val deptarget = dir.resolve(dep.relativeFinalization)
            if !Files.exists(deptarget) then
              Files.createDirectories(deptarget.getParent)
              Files.createLink(deptarget, dep.file.absolute)
              ()
        Await.result(Latexmk.latexmk(dir, contentHash, texfile).runToFuture(using ()), Duration.Inf)
    res match
      case Some(res) =>
        List(
          Directive(
            DCommand.Image,
            Attributes(List(
              Attribute(target.projectAbsolute.toString),
              Attribute("color", "autoinvert")
            ))
          )(block.prov)
        )
      case None =>
        Nil
}
