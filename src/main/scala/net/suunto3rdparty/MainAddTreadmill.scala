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

  val routeCoordXml = gpx \ "trk" \ "trkseg" \ "trkpt"

  val routeCoord = routeCoordXml.map { rc =>
    val lat = (rc \ "@lat").text.toDouble
    val lon = (rc \ "@lon").text.toDouble
    (lat, lon)
  }

  val distances = (routeCoord zip routeCoord.drop(1)).map {
    case (b, e) =>
      GPS.distance(b._1, b._2, e._1, e._2)

  }

  val totalDistance = distances.sum

  def getCoordAtDistance(dist: Double): (Double, Double) = {
    (routeCoord(0)._1, routeCoord(0)._2)
  }

  // load FIT, output as FIT again, only enriched with the GPX data


  def processFitFile(in: InputStream, out: IoFile): Unit = {
    val decode = new Decode
    val encode = new FileEncoder(out)
    try {
      val listener = new MesgListener {
        override def onMesg(mesg: Mesg): Unit = {
          encode.write(mesg)
          if (mesg.getName == "record") {
            // enrich distance records with GPS data
            val dist = mesg.getField("distance").getFloatValue
            val time = mesg.getField("timestamp").getLongValue
            //val time = mesg.getTimestamp
            val myMsg = new RecordMesg()
            val coord = getCoordAtDistance(dist.toDouble)
            val longLatScale = (1L << 31).toDouble / 180
            myMsg.setTimestamp(new DateTime(time))
            myMsg.setPositionLong((coord._2 * longLatScale).toInt)
            myMsg.setPositionLat((coord._1 * longLatScale).toInt)
            encode.write(myMsg)
          }
        }
      }
      decode.read(in, listener)
    } finally {
      in.close()
      encode.close()
    }
  }

  val is = new FileInputStream(progressPath)
  try {
    processFitFile(is, new IoFile(resultPath))
  } finally {
    is.close()
  }

}
