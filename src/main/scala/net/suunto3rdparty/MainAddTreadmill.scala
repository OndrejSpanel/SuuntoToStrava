package net.suunto3rdparty

import java.io.{FileInputStream, InputStream, File => IoFile}

import com.garmin.fit._

import scala.xml.XML

//import scala.collection.JavaConverters._

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

  val gpx = XML.loadFile(routePath)


  // load FIT, output as FIT again, only enriched with the GPX data


  def processFitFile(in: InputStream, out: IoFile): Unit = {
    val decode = new Decode
    val encode = new FileEncoder(out)
    try {
      val listener = new MesgListener {
        override def onMesg(mesg: Mesg): Unit = {
          println(mesg.getName)
          encode.write(mesg)
        }
      }
      decode.read(in, listener)
    } finally {
      in.close()
      encode.close()
    }
  }

  val routeCoordXml = gpx \ "trk" \ "trkseg" \ "trkpt"

  val routeCoord = routeCoordXml.map { rc =>
    val lat = (rc \ "@lat").text.toDouble
    val lon = (rc \ "@lon").text.toDouble
    (lon, lat)
  }

  val is = new FileInputStream(progressPath)
  try {
    processFitFile(is, new IoFile(resultPath))
  } finally {
    is.close()
  }

}
