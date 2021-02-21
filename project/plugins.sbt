lazy val bulkySourcesPlugin = RootProject(file("../plugins"))
lazy val root = (project in file(".")).dependsOn(bulkySourcesPlugin)

addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "1.1.0")