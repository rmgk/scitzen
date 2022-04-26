package scitzen.cli

import dev.dirs.ProjectDirectories
import dev.dirs.BaseDirectories
import dev.dirs.UserDirectories
import better.files.File
import scitzen.generic.ProjectConfig

class Environment(globalConfigDir: File, globalCacheDir: File, globalConfig: Option[ProjectConfig]) {}

object Environment {
  def apply(): Environment =
    val pd = ProjectDirectories.fromPath("scitzen")
    val cd = File(pd.configDir)
    val conf = cd / "scitzen.scim-conf"
    val config = Option.when(conf.exists){ ProjectConfig.parse(conf.contentAsString) }
    new Environment(cd, File(pd.cacheDir), config)
}
