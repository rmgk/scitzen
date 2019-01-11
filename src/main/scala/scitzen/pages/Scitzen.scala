package scitzen.pages

import com.monovore.decline.{CommandApp, Opts}

object Scitzen extends CommandApp(
  name = "scitzen",
  header = "Static page generator",
  main = {
    Opts.subcommand(Convert.command).orElse(Opts.subcommand(Renaming.command))
  }
)
