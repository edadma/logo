ThisBuild / licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme     := Some("semver-spec")

publish / skip := true

lazy val logo = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name         := "logo",
    version      := "0.0.1",
    scalaVersion := "3.5.2",
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:existentials",
        "-language:dynamics",
      ),
    organization                            := "io.github.edadma",
    githubOwner                             := "edadma",
    githubRepository                        := name.value,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "char-reader" % "0.1.15",
    ),
    libraryDependencies ++= Seq(
//      "com.github.scopt" %%% "scopt" % "4.1.0",
      "com.lihaoyi" %%% "pprint" % "0.9.0", /*% "test"*/
    ),
    publishMavenStyle      := true,
    Test / publishArtifact := false,
    licenses += "ISC"      -> url("https://opensource.org/licenses/ISC"),
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
    ),
  )
  .nativeSettings(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
  )
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    //    Test / scalaJSUseMainModuleInitializer := true,
    //    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer      := false,
    Test / scalaJSUseTestModuleInitializer      := true,
    scalaJSUseMainModuleInitializer             := true,
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
  )
