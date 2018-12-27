ThisBuild / scalaVersion := "2.12.8"
ThisBuild / organization := "de.rmgk"



lazy val vitzen = project.in(file("vitzen"))
                  .enablePlugins(SbtSassify)
                  .enablePlugins(JavaAppPackaging)
                  .settings(
                    name := "vitzen",
                    Compile / compile := ((Compile / compile) dependsOn (Assets / SassKeys.sassify)).value,
                    vitzendeps
                  )
                  .dependsOn(asciimedic)
//Compile / resources ++= (Assets / SassKeys.sassify).value

lazy val asciimedic = project.in(file("asciimedic"))
                      .settings(
                        name := "asciimedic",
                        asciimedicdeps
                      )





lazy val asciimedicdeps = libraryDependencies ++= Seq(
  "com.lihaoyi" %% "scalatags" % "0.6.7",
  "com.monovore" %% "decline" % "0.5.0",
  "com.github.pathikrit" %% "better-files" % "3.6.0",
  "com.lihaoyi" %% "fastparse" % "2.0.4",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
  "com.lihaoyi" %% "pprint" % "0.5.3"
)



lazy val vitzendeps = libraryDependencies ++= Seq(
  "org.asciidoctor" % "asciidoctorj" % "1.6.0-alpha.7",
  "com.lihaoyi" %% "scalatags" % "0.6.7",
  "com.monovore" %% "decline" % "0.5.0",
  "org.webjars.npm" % "normalize.css" % "8.0.0",
  "org.webjars.bower" % "highlightjs" % "9.12.0",
  "org.webjars" % "webjars-locator-core" % "0.35",
  "com.github.pathikrit" %% "better-files" % "3.6.0",
  "org.jsoup" % "jsoup" % "1.11.3"
)

ThisBuild / Compile / compile / scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-feature",
  "-Xlint",
  "-Xfuture",
  //"-Xlog-implicits" ,
  "-Xfatal-warnings",
  //"-Yinline-warnings" ,
  "-Yno-adapted-args",
  //"-Ywarn-dead-code" ,
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-numeric-widen",
  //"-Ywarn-value-discard" ,
  )
