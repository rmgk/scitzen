name := "asciimedic"
scalaVersion := "2.12.6"
organization := "de.rmgk"

enablePlugins(JavaAppPackaging)

libraryDependencies ++= Seq(
  "org.asciidoctor" % "asciidoctorj" % "1.6.0-alpha.7",
  "com.lihaoyi" %% "scalatags" % "0.6.7",
  "com.monovore" %% "decline" % "0.5.0",
  "com.github.pathikrit" %% "better-files" % "3.6.0",
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
  "com.lihaoyi" %% "pprint" % "0.5.3"
)

Compile / compile / scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-feature",
  "-Xlint",
  "-Xfuture",
  //"-Xlog-implicits" ,
  //"-Yno-predef" ,
  //"-Yno-imports" ,
  "-Xfatal-warnings",
  //"-Yinline-warnings" ,
  "-Yno-adapted-args",
  //"-Ywarn-dead-code" ,
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-numeric-widen",
  //"-Ywarn-value-discard" ,
)
