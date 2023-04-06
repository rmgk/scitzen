package scitzen.cli

import dev.dirs.ProjectDirectories
import dev.dirs.BaseDirectories
import dev.dirs.UserDirectories
import scitzen.generic.ProjectConfig

import java.nio.file.{Files, Path, Paths}

class Environment(globalConfigDir: Path, globalCacheDir: Path, globalConfig: Option[ProjectConfig]) {}

object Environment {
  def apply(): Environment =
    val pd     = ProjectDirectories.fromPath("scitzen")
    val cd     = Path.of(pd.configDir)
    val conf   = cd.resolve("scitzen.scim-conf")
    val config = Option.when(Files.exists(conf)) { ProjectConfig.parse(Files.readAllBytes(conf)) }
    new Environment(cd, Path.of(pd.cacheDir), config)
}
