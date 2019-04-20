import Settings._
import Dependencies._
import org.irundaia.sass.Maxified
import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.CrossType


ThisBuild / organization := "de.rmgk"
name := "scitzen"
inThisBuild(scalaVersion_212)

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
                  cats,
                  scribe
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
                 name := "scitzen",
                 Compile / resources ++= (webResources / Assets / SassKeys.sassify).value,
                 resolvers += Resolver.sonatypeRepo("public"),
                 libraryDependencies ++= Seq(
                   "de.undercouch" % "citeproc-java" % "1.0.1",
                   "org.citationstyles"% "styles" % "1.0.1-SNAPSHOT",
                   "org.citationstyles"% "locales" % "1.0.1-SNAPSHOT"
                   ),
                 strictCompile,
                 decline,
                 betterFiles,
                 publishLocal := publishLocal.dependsOn(coreJVM / publishLocal).value
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