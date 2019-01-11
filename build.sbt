import Settings._
import Dependencies._

lazy val scitzen = project.in(file("."))
                   .enablePlugins(SbtSassify)
                   .enablePlugins(JavaAppPackaging)
                   .settings(
                     name := "scitzen",
                     scalaVersion := version_212,
                     organization := "de.rmgk",
                     Compile / compile := ((Compile / compile) dependsOn (Assets / SassKeys.sassify)).value,
                     scalatags,
                     decline,
                     betterFiles,
                     jsoup,
                     compileWithStrictScalacOptions,
                     fastparse,
                     scalatest,
                     scalacheck,
                     pprint,
                     normalizecss
                   )
