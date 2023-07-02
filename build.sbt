import Dependencies._
import Settings._

lazy val scitzen = project.in(file("."))
  .settings(
    name         := "scitzen",
    organization := "de.rmgk",
    scalaVersion_3,
    resolverJitpack,
    resolverS01,
    libraryDependencies ++= Seq(
      jsoup.value,
      directories.value,
      upickle.value,
      slips.chain.value,
      slips.logging.value,
      slips.scip.value,
      slips.options.value,
      scopt.value,
      jsoniterScala.value,
      munit.value,
      "com.github.fomkin"             %%% "levsha-core"   % "1.3.0",
      "org.webjars.bowergithub.prismjs" % "prism"         % "1.29.0",
      "org.webjars.npm"                 % "katex"         % "0.16.4",
      "org.jbibtex"                     % "jbibtex"       % "1.0.20",
      "de.undercouch"                   % "citeproc-java" % "2.0.0",
      "org.graalvm.js" % "js" % "23.0.0", // explicitly depend on graal.js to allow running on non-graal JVMs
    ),
  )
