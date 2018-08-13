val ver = "0.1"

name := "qiniu-uploader"

version := ver

libraryDependencies ++= Seq(
  "org.rogach" %% "scallop" % "3.1.2",
  "io.argonaut" %% "argonaut" % "6.2.1",
  "com.qiniu" % "qiniu-java-sdk" % "7.2.+",
  "com.lihaoyi" %% "upickle" % "0.6.6",
)


scalaVersion := "2.12.6"

lazy val commonSettings = Seq(
  version := ver,
  organization := "com.lls",
  scalaVersion := "2.10.1",
  test in assembly := {}
)

lazy val app = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    mainClass in assembly := Some("com.lls.qiniu.Uploader"),
    assemblyJarName in assembly := s"qiniu-uploader-$ver.jar",
  )
