inThisBuild(
  Seq(
    organization := "zone.slice",
    scalaVersion := "2.13.1",
  ),
)

val dependencies = Seq(
  "com.lihaoyi" %% "fastparse" % "2.2.2",
)

lazy val bezier = (project in file("."))
  .settings(
    name := "bezier",
    libraryDependencies ++= dependencies,
    Compile / scalaSource := baseDirectory.value / "src",
  )
