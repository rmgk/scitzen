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

lazy val webResources = project.in(file("resources"))
                        .enablePlugins(SbtSassify)
                        .settings(
  SassKeys.cssStyle := Maxified,
  normalizecss
)


lazy val cli = project.in(file("cli"))
               .enablePlugins(JavaAppPackaging)
               .enablePlugins(GraalVMNativeImagePlugin)
               .dependsOn(coreJVM)
               .settings(
                 name := "scitzen-cli",
                 Compile / resources ++= (webResources / Assets / SassKeys.sassify).value,
                 strictCompile,
                 decline,
                 betterFiles,
                 libraryDependencies += "org.scalameta" %% "mdoc" % "1.2.10",
                 Resolvers.stg,
                 libraryDependencies += "de.tuda.stg" %% "rescala" % "0.24.0"
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