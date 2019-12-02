
lazy val root = (project in file("."))
  .settings(
    name := "Sit",
    scalaVersion := "2.13.1",
    connectInput in run := true,
    libraryDependencies += "net.liftweb" %% "lift-json" % "3.4.0"
  )