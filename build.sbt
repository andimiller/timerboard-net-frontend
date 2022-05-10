import scala.sys.process._
import scala.language.postfixOps

import sbtwelcome._

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"

lazy val timerboard =
  (project in file("."))
    .enablePlugins(ScalaJSPlugin)
    .settings( // Normal settings
      name              := "timerboard",
      version           := "0.0.1",
      scalaVersion      := "3.1.1",
      organization      := "net.timerboard",
      libraryDependencies ++= Seq(
        "io.indigoengine"              %%% "tyrian"        % "0.3.2",
        "io.circe"                     %%% "circe-parser"  % "0.14.1",
        "io.circe"                     %%% "circe-generic" % "0.14.1",
        "com.softwaremill.magnolia1_3" %%% "magnolia"      % "1.1.2",
        "org.gnieh"                    %%% "diffson-circe" % "4.1.1",
        "co.fs2" %%% "fs2-io" % "3.2.7",
        "io.github.cquiroz" %%% "scala-java-time" % "2.4.0-M2",
        "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.4.0-M2",
        "net.andimiller" %%% "hedgehogs-core" % "0.2.0",
        "net.andimiller" %%% "hedgehogs-circe" % "0.2.0",
        "org.typelevel" %%% "munit-cats-effect-3" % "1.0.5" % Test,
        "org.scalameta"                %%% "munit"         % "0.7.29" % Test,
        "net.andimiller" %%% "munit-cats-effect-3-styles" % "1.0.1" % Test
      ),
      testFrameworks += new TestFramework("munit.Framework"),
      scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
      scalafixOnCompile := true,
      semanticdbEnabled := true,
      semanticdbVersion := scalafixSemanticdb.revision,
      autoAPIMappings   := true
    )
    .settings( // Launch VSCode when you type `code` in the sbt terminal
      code := {
        val command = Seq("code", ".")
        val run     = sys.props("os.name").toLowerCase match {
          case x if x contains "windows" => Seq("cmd", "/C") ++ command
          case _                         => command
        }
        run.!
      }
    )
    .settings( // Welcome message
      logo             := "timerboard.net (v" + version.value + ")",
      usefulTasks      := Seq(
        UsefulTask("", "fastOptJS", "Rebuild the JS (use during development)"),
        UsefulTask(
          "",
          "fullOptJS",
          "Rebuild the JS and optimise (use in production)"
        ),
        UsefulTask("", "code", "Launch VSCode")
      ),
      logoColor        := scala.Console.MAGENTA,
      aliasColor       := scala.Console.BLUE,
      commandColor     := scala.Console.CYAN,
      descriptionColor := scala.Console.WHITE
    )

lazy val code =
  taskKey[Unit]("Launch VSCode in the current directory")
