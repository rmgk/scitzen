import Settings._
import Dependencies._
import org.irundaia.sass.Maxified

lazy val scitzen = project.in(file("."))
                   .enablePlugins(SbtSassify)
                   .enablePlugins(JavaAppPackaging)
                   .settings(
                     name := "scitzen",
                     scalaVersion := version_212,
                     organization := "de.rmgk",
                     Compile / compile := ((Compile / compile) dependsOn (Assets / SassKeys.sassify)).value,
                     SassKeys.cssStyle := Maxified,
                     scalatags,
                     decline,
                     betterFiles,
                     jsoup,
                     compileWithStrictScalacOptions,
                     fastparse,
                     scalatest,
                     scalacheck,
                     pprint,
                     normalizecss,
                     rmgkLogging
                   )
