name := "vitzen"
scalaVersion := "2.12.6"

enablePlugins(JavaAppPackaging)
enablePlugins(SbtSassify)

Compile / compile := ((compile in Compile) dependsOn (Assets / SassKeys.sassify)).value
//Compile / resources ++= (Assets / SassKeys.sassify).value

libraryDependencies ++= Seq(
  "org.asciidoctor" % "asciidoctorj" % "1.6.0-alpha.7",
  "com.lihaoyi" %% "scalatags" % "0.6.7",
  "com.monovore" %% "decline" % "0.4.2",
  "org.webjars.npm" % "normalize.css" % "8.0.0",
  "org.webjars.bower" % "highlightjs" % "9.12.0",
  "org.webjars" % "webjars-locator-core" % "0.35",
  "com.github.pathikrit" %% "better-files" % "3.6.0",
  "org.jsoup" % "jsoup" % "1.11.3",
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
