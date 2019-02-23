import Settings._
import Dependencies._
import org.irundaia.sass.Maxified
import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.CrossType

lazy val scitzen = crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full).in(file("."))
                   .settings(
                     name := "scitzen",
                     scalaVersion := version_212,
                     organization := "de.rmgk",
                     scalatags,
                     decline,
                     betterFiles,
                     jsoup,
                     compileWithStrictScalacOptions,
                     fastparse,
                     scalatest,
                     scalacheck,
                     pprint,
                     rmgkLogging
                   ).jvmConfigure(
  p => p
       .enablePlugins(SbtSassify)
       .enablePlugins(JavaAppPackaging)
       .settings(
         Compile / compile := ((Compile / compile) dependsOn (Assets / SassKeys.sassify)).value,
         SassKeys.cssStyle := Maxified,
         normalizecss
       )
)
                   .jsSettings(
                     scalaJSUseMainModuleInitializer := true,
                     scalajsdom
                   )

lazy val scitzenJS = scitzen.js
lazy val scitzenJVM = scitzen.jvm