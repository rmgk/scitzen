import Settings._
import Dependencies._
import org.irundaia.sass.Maxified
import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.CrossType


ThisBuild / organization := "de.rmgk"
name := "scitzen"
scalaVersion_212

lazy val core = crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Pure)
                .in(file("core"))
                .settings(
                  name := "scitzen-core",
                  scalatags,
                  strictCompile,
                  fastparse,
                  scalatest,
                  scalacheck,
                  pprint,
                  rmgkLogging,
                  cats
                )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js


lazy val cli = project.in(file("cli"))
               .enablePlugins(SbtSassify)
               .enablePlugins(JavaAppPackaging)
               .enablePlugins(GraalVMNativeImagePlugin)
               .dependsOn(coreJVM)
               .settings(
                 name := "scitzen-cli",
                 Compile / compile := ((Compile / compile) dependsOn (Assets / SassKeys.sassify)).value,
                 SassKeys.cssStyle := Maxified,
                 normalizecss,
                 strictCompile,
                 decline,
                 betterFiles,
                 libraryDependencies += "org.scalameta" %% "mdoc" % "1.2.10"
               )

lazy val web = project.in(file("web"))
               .enablePlugins(ScalaJSPlugin)
               .dependsOn(coreJS)
               .settings(
                 name := "scitzen-web",
                 scalaJSUseMainModuleInitializer := true,
                 scalajsdom,
                 strictCompile,
                 )