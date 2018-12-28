import Settings._
import Dependencies._

ThisBuild / scalaVersion := version_212
ThisBuild / organization := "de.rmgk"


lazy val vitzen = project.in(file("vitzen"))
                  .enablePlugins(SbtSassify)
                  .enablePlugins(JavaAppPackaging)
                  .settings(
                    name := "vitzen",
                    Compile / compile := ((Compile / compile) dependsOn (Assets / SassKeys.sassify)).value,
                    vitzendeps,
                    scalatags,
                    decline,
                    betterFiles,
                    jsoup,
                    compileWithStrictScalacOptions
                  )
                  .dependsOn(asciimedic)
//Compile / resources ++= (Assets / SassKeys.sassify).value

lazy val asciimedic = project.in(file("asciimedic"))
                      .settings(
                        name := "asciimedic",
                        scalatags,
                        decline,
                        betterFiles,
                        fastparse,
                        scalatest,
                        scalacheck,
                        pprint,
                        compileWithStrictScalacOptions
                      )

lazy val vitzendeps = libraryDependencies ++= Seq(
  "org.asciidoctor" % "asciidoctorj" % "1.6.0-alpha.7",
  "org.webjars.npm" % "normalize.css" % "8.0.0",
  "org.webjars.bower" % "highlightjs" % "9.12.0",
  "org.webjars" % "webjars-locator-core" % "0.35"
)
