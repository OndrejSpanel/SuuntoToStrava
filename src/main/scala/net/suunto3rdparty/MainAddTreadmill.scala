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
    if (ext < 0) progressPath + suffix
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

  val diffs = (routeCoord zip routeCoord.drop(1)).map {
    case (b, e) =>
      GPS.distance(b._1, b._2, e._1, e._2)
  }

  val distances = diffs.scanLeft(0.0) { case (sum, d) =>
    sum + d
  }

  val gpsDistance = distances.last


  def getCoordAtDistance(dist: Double): (Double, Double) = {
    val i = distances.indexWhere(_ >= dist)
    // TODO: interpolation
    if (i < 0) routeCoord.last
    else if (i > 0) {
      def lerp(a: Double, b: Double, f: Double) = a + (b - a) * f

      def lerpC(a: (Double, Double), b: (Double, Double), f: Double) = (lerp(a._1, b._1, f), lerp(a._2, b._2, f))

      val f = (dist - distances(i - 1)) / (distances(i) - distances(i - 1))
      lerpC(routeCoord(i - 1), routeCoord(i), f)
    } else routeCoord(i)
  }

  // load FIT, output as FIT again, only enriched with the GPX data

  def readDistance(in: InputStream): Double = {
    trait LastDist {
      def lastDist: Float
    }
    val listener = new MesgListener with LastDist {
      var lastDist = 0f

      override def onMesg(mesg: Mesg): Unit = {
        if (mesg.getName == "record") {
          val dist = mesg.getField("distance").getFloatValue
          lastDist = dist
        }
      }
    }

    val decode = new Decode
    decode.read(in, listener)
    listener.lastDist
  }

  val fitDistance = {
    val is = new FileInputStream(progressPath)
    try {
      readDistance(is)
    } finally {
      is.close()
    }
  }

  def processFitFile(in: InputStream, out: IoFile): Unit = {
    val decode = new Decode
    val encode = new FileEncoder(out)
    try {
      val listener = new MesgListener {
        override def onMesg(mesg: Mesg): Unit = {
          if (mesg.getName == "record") {
            // TODO: fix distance to match GPS data
            encode.write(mesg)
            // enrich distance records with GPS data
            val dist = mesg.getField("distance").getFloatValue
            val fixedDist = dist / fitDistance * gpsDistance
            val time = mesg.getField("timestamp").getLongValue
            //val time = mesg.getTimestamp
            val myMsg = new RecordMesg()
            val coord = getCoordAtDistance(fixedDist.toDouble)
            val longLatScale = (1L << 31).toDouble / 180
            myMsg.setTimestamp(new DateTime(time))
            myMsg.setPositionLong((coord._2 * longLatScale).toInt)
            myMsg.setPositionLat((coord._1 * longLatScale).toInt)
            encode.write(myMsg)
          } else {
            encode.write(mesg)
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
