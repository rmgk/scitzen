import Dependencies._
import Settings._
import org.irundaia.sass.Maxified

lazy val scitzen = project.in(file("."))
                   .enablePlugins(JavaAppPackaging)
                   .enablePlugins(GraalVMNativeImagePlugin)
                   .enablePlugins(SbtSassify)
                   .settings(
                     name := "scitzen",
                     organization := "de.rmgk",
                     scalaVersion_212,
                     Compile / resources ++= (Assets / SassKeys.sassify).value,
                     resolvers += Resolver.sonatypeRepo("public"),
                     libraryDependencies ++= Seq(
                       "de.undercouch" % "citeproc-java" % "1.0.1",
                       "com.vdurmont" % "emoji-java" % "4.0.0"
                       //"org.citationstyles"% "styles" % "1.0.1-SNAPSHOT",
                       //"org.citationstyles"% "locales" % "1.0.1-SNAPSHOT"
                       ),
                     strictCompile,
                     decline,
                     betterFiles,
                     scalatags,
                     strictCompile,
                     fastparse,
                     scalatest,
                     scalacheck,
                     pprint,
                     cats,
                     scribe,
                     SassKeys.cssStyle := Maxified,
                     normalizecss
                     )
