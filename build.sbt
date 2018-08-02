name := "vitzen"
scalaVersion := "2.12.6"

enablePlugins(JavaAppPackaging)

libraryDependencies ++= Seq(
  "org.asciidoctor" % "asciidoctorj" % "1.6.0-alpha.7",
  "com.lihaoyi" %% "scalatags" % "0.6.7"
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
