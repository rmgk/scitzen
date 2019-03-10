import Settings._
import Dependencies._
import org.irundaia.sass.Maxified
import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.CrossType

ThisBuild / scalaVersion := version_212
ThisBuild / organization := "de.rmgk"
name := "scitzen"


lazy val scitzenCore = crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Pure)
                       .in(file("core"))
                       .settings(
                         name := "scitzen-core",
                         scalatags,
                         decline,
                         compileWithStrictScalacOptions,
                         fastparse,
                         scalatest,
                         scalacheck,
                         pprint,
                         rmgkLogging
                       )

lazy val scitzenCoreJVM = scitzenCore.jvm
lazy val scitzenCoreJS = scitzenCore.js


lazy val scitzenCli = project.in(file("cli"))
                      .enablePlugins(SbtSassify)
                      .enablePlugins(JavaAppPackaging)
                      .dependsOn(scitzenCoreJVM)
                      .settings(
                        name := "scitzen-cli",
                        Compile / compile := ((Compile / compile) dependsOn (Assets / SassKeys.sassify)).value,
                        SassKeys.cssStyle := Maxified,
                        normalizecss,
                        compileWithStrictScalacOptions,
                        betterFiles
                      )

lazy val scitzenWeb = project.in(file("web"))
                      .enablePlugins(ScalaJSPlugin)
                      .dependsOn(scitzenCoreJS)
                      .settings(
                        name := "scitzen-web",
                        scalaJSUseMainModuleInitializer := true,
                        scalajsdom,
                        compileWithStrictScalacOptions
                      )