package net.savinko
package _6_sbt.src.main.scala

import sbt.Keys.sources
import sbt.{AutoPlugin, Compile, File, Setting, Test, settingKey, taskKey}

object BulkySourcesPlugin extends AutoPlugin {
  override def trigger = allRequirements

  val bulkyThresholdInLines = settingKey[Int]("threshold in lines")
  val bulkySources = taskKey[Seq[(Int, File)]]("file seq with >= lines of code")

  override lazy val globalSettings: Seq[Setting[_]] = Seq(bulkyThresholdInLines := 100)

  def getBulkySources(files: Seq[File], filterMoreThan: Int): Seq[(Int, File)] = {
    files.map { file => (sbt.IO.readLines(file).length, file) }
      .filter { case (lines, _) => lines > filterMoreThan }
      .sortWith { (current, next) => current._1 > next._1 }
  }

  override val projectSettings: Seq[Setting[_]] = Seq(
    bulkySources := getBulkySources((sources in Compile).value, bulkyThresholdInLines.value),
    (Test / bulkySources) := getBulkySources((sources in Test).value, bulkyThresholdInLines.value)
  )
}
