lazy val root = (project in file("."))
  .enablePlugins(SbtPlugin)
  .settings(
    name := "bulky-plugin",
    scalaVersion := "2.12.10", // 2.13 does not work???
    version := "1.0"
  )


