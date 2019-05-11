package scitzen.cli

import com.monovore.decline.{CommandApp, Opts}
import scribe.Logger

object Scitzen extends CommandApp(
  name = "scitzen",
  header = "Static page generator",
  main = {

    import scribe.format._
    val myFormatter: Formatter = formatter"$message ($position)"
    Logger.root.clearHandlers().withHandler(formatter = myFormatter,
                                            minimumLevel = Some(scribe.Level.Info)).replace()

    Opts.subcommand(Convert.command)
    .orElse(Opts.subcommand(JsonSast.command))
    .orElse(Opts.subcommand(Format.command))
  }
  )
