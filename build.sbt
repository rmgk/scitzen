import Dependencies._
import Settings._

lazy val scitzen = project.in(file("."))
  .settings(
    name         := "scitzen",
    organization := "de.rmgk",
    scalaVersion_3,
    libraryDependencies ++= Seq(
      jsoup.value,
      slips.chain.value,
      slips.logging.value,
      slips.scip.value,
      slips.options.value,
      slips.script.value,
      scopt.value,
      jsoniterScala.value,
      munit.value,
      "org.webjars.bowergithub.prismjs" % "prism"         % "1.29.0",
      "org.webjars.npm"                 % "katex"         % "0.16.4",
      "org.jbibtex"                     % "jbibtex"       % "1.0.20",
      "de.undercouch"                   % "citeproc-java" % "2.0.0",
      "org.graalvm.js" % "js" % "23.0.0", // explicitly depend on graal.js to allow running on non-graal JVMs
    ),
  )
