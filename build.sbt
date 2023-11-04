import Dependencies._
import Settings._

lazy val scitzen = project.in(file("."))
  .settings(
    name         := "scitzen",
    organization := "de.rmgk",
    scalaVersion_3,
    Dependencies.jsoup,
    Dependencies.slips.chain,
    Dependencies.slips.logging,
    Dependencies.slips.scip,
    Dependencies.slips.options,
    Dependencies.slips.script,
    Dependencies.jsoniterScala,
    munit,
    libraryDependencies ++= Seq(
      "org.webjars.bowergithub.prismjs" % "prism"   % "1.29.0",
      "org.webjars.npm"                 % "katex"   % "0.16.4",
      "org.jbibtex"                     % "jbibtex" % "1.0.20",
      "de.undercouch" % "citeproc-java" % "2.0.0" exclude ("org.graalvm.js", "js") exclude ("org.graalvm.sdk", "graal-sdk"),
      "org.graalvm.polyglot" % "polyglot"        % "23.1.0",
      "org.graalvm.polyglot" % "js-community"    % "23.1.0",
      "org.graalvm.polyglot" % "tools-community" % "23.1.0",
      // "org.graalvm.js" % "js" % "23.0.2", // explicitly depend on graal.js to allow running on non-graal JVMs
    ),
  )
