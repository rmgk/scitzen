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
                    scitzendependencies,
                    scalatags,
                    decline,
                    betterFiles,
                    jsoup,
                    compileWithStrictScalacOptions,
                    fastparse,
                    scalatest,
                    scalacheck,
                    pprint
                  )
//Compile / resources ++= (Assets / SassKeys.sassify).value


lazy val scitzendependencies = libraryDependencies ++= Seq(
  "org.asciidoctor" % "asciidoctorj" % "1.6.0",
  "org.webjars.npm" % "normalize.css" % "8.0.1",
  "org.webjars.bower" % "highlightjs" % "9.12.0",
  "org.webjars" % "webjars-locator-core" % "0.36"
)
