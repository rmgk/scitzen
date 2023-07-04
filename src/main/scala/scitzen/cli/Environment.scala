package scitzen.cli

import dev.dirs.ProjectDirectories
import scitzen.generic.ProjectConfig

import java.nio.file.{Files, Path}
import scala.annotation.unused

class Environment(
    @unused globalConfigDir: Path,
    @unused globalCacheDir: Path,
    @unused globalConfig: Option[ProjectConfig]
) {}

object Environment {
  def apply(): Environment =
    val pd     = ProjectDirectories.fromPath("scitzen")
    val cd     = Path.of(pd.configDir)
    val conf   = cd.resolve("scitzen.scim-conf")
    val config = Option.when(Files.exists(conf)) { ProjectConfig.parse(Files.readAllBytes(conf)) }
    new Environment(cd, Path.of(pd.cacheDir), config)
}
