import Dependencies._


bintrayRepository := "ntile"
bintrayOmitLicense := true
bintrayVcsUrl := Some("git@github.com:you/your-repo.git")

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.riversoft.ntile",
      scalaVersion := "2.12.6",
      version := "2.4.2"
    )),
    name := "ntile-expression-translator",
    libraryDependencies ++= Seq(
      "org.parboiled" %% "parboiled" % "2.1.4",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.8.0",
      scalaTest % Test
    )
  )
