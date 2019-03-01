import sbt.Keys._

name := "paradise"

val scalaV = "2.12.6"
//val scalaV = "2.11.8"

val projectName = "paradise"
val projectVersion = "20190122"

val projectMainClass = "org.seekloud.paradise.Boot"
val clientMainClass = "org.seekloud.paradise.ClientBoot"

def commonSettings = Seq(
  version := projectVersion,
  scalaVersion := scalaV,
  scalacOptions ++= Seq(
    //"-deprecation",
    "-feature"
  )
)

// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.{crossProject, CrossType}


lazy val shared = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("shared"))
  .settings(name := "shared")
  .settings(commonSettings: _*)

lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

// Scala-Js frontend
lazy val frontend = (project in file("frontend"))
  .enablePlugins(ScalaJSPlugin)
  .settings(name := "frontend")
  .settings(commonSettings: _*)
  .settings(
    inConfig(Compile)(
      Seq(
        fullOptJS,
        fastOptJS,
        packageJSDependencies,
        packageMinifiedJSDependencies
      ).map(f => (crossTarget in f) ~= (_ / "sjsout"))
    ))
  .settings(skip in packageJSDependencies := false)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    //mainClass := Some("com.neo.sk.virgour.front.Main"),
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % "0.8.0",
      "io.circe" %%% "circe-generic" % "0.8.0",
      "io.circe" %%% "circe-parser" % "0.8.0",
      "org.scala-js" %%% "scalajs-dom" % "0.9.2",
      "io.suzaku" %%% "diode" % "1.1.2",
      //"com.lihaoyi" %%% "upickle" % "0.6.6",
      "com.lihaoyi" %%% "scalatags" % "0.6.5",
      //"org.scala-js" %%% "scalajs-java-time" % scalaJsJavaTime
      //"com.lihaoyi" %%% "utest" % "0.3.0" % "test"
      "org.seekloud" %%% "byteobject" % "0.1.1"
    )
  )
  .dependsOn(sharedJs)

// Akka Http based backend
lazy val backend = (project in file("backend")).enablePlugins(PackPlugin)
  .settings(commonSettings: _*)
  .settings(
    mainClass in reStart := Some(projectMainClass),
    javaOptions in reStart += "-Xmx2g"
  )
  .settings(name := "paradise")
  .settings(
    //pack
    // If you need to specify main classes manually, use packSettings and packMain
    //packSettings,
    // [Optional] Creating `hello` command that calls org.mydomain.Hello#main(Array[String])
    packMain := Map("paradise" -> projectMainClass),
    packJvmOpts := Map("paradise" -> Seq("-Xmx256m", "-Xms64m")),
    packExtraClasspath := Map("paradise" -> Seq("."))
  )
  .settings(
    libraryDependencies ++= Dependencies.backendDependencies
  )
//  .settings {
//    (resourceGenerators in Compile) += Def.task {
//      val fastJsOut = (fastOptJS in Compile in frontend).value.data
//      val fastJsSourceMap = fastJsOut.getParentFile / (fastJsOut.getName + ".map")
//      Seq(
//        fastJsOut,
//        fastJsSourceMap
//      )
//    }.taskValue
//  }
    .settings(
      (resourceGenerators in Compile) += Def.task {
        val fullJsOut = (fullOptJS in Compile in frontend).value.data
        val fullJsSourceMap = fullJsOut.getParentFile / (fullJsOut.getName + ".map")
        Seq(
          fullJsOut,
          fullJsSourceMap
        )
      }.taskValue)
  .settings((resourceGenerators in Compile) += Def.task {
  Seq(
    (packageJSDependencies in Compile in frontend).value
    //(packageMinifiedJSDependencies in Compile in frontend).value
  )
}.taskValue)
  .settings(
    (resourceDirectories in Compile) += (crossTarget in frontend).value,
    watchSources ++= (watchSources in frontend).value
  )
  .dependsOn(sharedJvm)

//javafx client
lazy val client = (project in file("client")).enablePlugins(PackPlugin)
  .settings(name := "client")
  .settings(commonSettings: _*)
    .settings(
      mainClass in reStart := Some(clientMainClass),
      javaOptions in reStart += "-Xmx2g"
    )
  .settings(
    packMain := Map("paradise" -> clientMainClass),
    packJvmOpts := Map("paradise" -> Seq("-Xmx512m", "-Xms64m")),
    packExtraClasspath := Map("paradise" -> Seq("."))
  )
  .settings(libraryDependencies ++= Dependencies.backendDependencies)
//  .settings(
//    PB.targets in Compile := Seq(
//      scalapb.gen() -> (sourceManaged in Compile).value
//    )
//  )
  .dependsOn(sharedJvm)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(frontend, backend)
  .settings(name := "root")


