lazy val defaults = Seq(
  // scala compiler version:
  scalaVersion := "2.12.4",
  scalaBinaryVersion := "2.12",
  scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked"),

  // source location configuration:
  scalaSource in Compile := baseDirectory.value / "src",
  scalaSource in Test := baseDirectory.value / "test",

  outputStrategy := Some(StdoutOutput),

  // run configuration:
  run / fork := true,
  run / javaOptions ++= Seq("-Xss128M", "-Xms128M"),

  // test configuration:
  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,
  testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),
  Test / parallelExecution := true,
  Test / fork := true,
  Test / javaOptions ++= Seq("-Xss128M", "-Xms128M"),
)

lazy val miniscalaBench = (project in file(".")).settings(
  defaults,
  name := "MiniScalaBenchmarksGame")
