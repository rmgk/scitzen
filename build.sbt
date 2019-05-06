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
                       "com.vdurmont" % "emoji-java" % "4.0.0"
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
                     upickle,
                     scribe,
                     kaleidoscope,
                     SassKeys.cssStyle := Maxified,
                     normalizecss
                     )
