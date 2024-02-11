import Dependencies.*
import Settings.*

lazy val scitzen = project.in(file("."))
  .settings(
    name         := "scitzen",
    organization := "de.rmgk",
    scala3defaults,
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
      "org.webjars.npm"                 % "katex"   % "0.16.9",
      "org.jbibtex"                     % "jbibtex" % "1.0.20",
      "de.undercouch" % "citeproc-java" % "2.0.0" exclude ("org.graalvm.js", "js") exclude ("org.graalvm.sdk", "graal-sdk"),
      "org.graalvm.polyglot" % "polyglot"        % "23.1.2",
      "org.graalvm.polyglot" % "js-community"    % "23.1.2",
      "org.graalvm.polyglot" % "tools-community" % "23.1.2",
    ),
  )
