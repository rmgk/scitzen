package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files.File
import com.monovore.decline.{Command, Opts}
import scitzen.converter.SastToScimConverter
import scitzen.semantics.SastConverter

object Format {

  implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8



  val command: Command[Unit] = Command(name = "format",
                                       header = "Convert Scim to Scim") {

    Opts.arguments[Path](metavar = "paths").map {
      _.map(File(_))
       .filter(_.isRegularFile)
       .foreach { file =>
         val content = file.contentAsString
         val sast = new SastConverter(Convert.includeBelow(file)).documentString(content)
         val result = SastToScimConverter.toScim(sast)
         file.write(result.mkString("\n"))
       }
    }

  }

}
