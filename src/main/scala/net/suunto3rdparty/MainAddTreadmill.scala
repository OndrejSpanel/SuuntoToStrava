package net.suunto3rdparty

object MainAddTreadmill extends App {
  val routePath = args(0)
  val progressPath = args(1)


  def createResultPath(s: String, suffix: String) = {

    val ext = progressPath.lastIndexOf('.')
    if (ext<0) progressPath + suffix
    else progressPath.take(ext) + suffix + progressPath.drop(ext)
  }
  val resultPath = createResultPath(progressPath, "_track")
  // parse GPX


  // load FIT, output as FIT again, only enriched with the GPX data
  println(resultPath)
}
