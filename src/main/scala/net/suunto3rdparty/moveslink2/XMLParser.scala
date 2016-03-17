package net.suunto3rdparty
package moveslink2

import java.io._
import java.time.{ZoneOffset, ZonedDateTime, ZoneId}
import java.time.format.DateTimeFormatter

import org.apache.commons.math.ArgumentOutsideDomainException
import org.apache.commons.math.analysis.interpolation.SplineInterpolator
import org.apache.commons.math.analysis.polynomials.PolynomialSplineFunction
import org.apache.log4j.Logger

import scala.collection.immutable.SortedMap
import scala.util._
import scala.xml._
import Util._

object XMLParser {
  private val log = Logger.getLogger(XMLParser.getClass)
  private val PositionConstant = 57.2957795131

  private val dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'").withZone(ZoneOffset.UTC)
  private val dateFormatNoZone = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").withZone(ZoneId.systemDefault)

  def interpolate(spline: PolynomialSplineFunction, x: Double): Double = {
    try {
      spline.value(x)
    }
    catch {
      case _: ArgumentOutsideDomainException =>
        val knots = spline.getKnots
        spline.value(knots(if (x < knots(0)) 0 else spline.getN - 1))
    }
  }
  def generateTimeToDistanceSplineFunction(timeArray: Seq[Double], distanceArray: Seq[Double]): PolynomialSplineFunction = {
    val interpolator = new SplineInterpolator
    interpolator.interpolate(timeArray.toArray, distanceArray.toArray)
  }
  def populateDistanceArray(distanceList: Seq[Double]): Seq[Double] = {
    for (d <- distanceList) yield d
  }

  def populateHRArray(hrList: Seq[Double]): Seq[Double] = {
    for (hr <- hrList) yield hr * 60
  }
  def generateTimeToHRSplineFunction(timeArray: Seq[Double], hrArray: Seq[Double]): PolynomialSplineFunction = {
    val interpolator = new SplineInterpolator
    interpolator.interpolate(timeArray.toArray, hrArray.toArray)
  }

  def getXMLDocument(xmlFile: File): Elem = {
    XML.loadFile(xmlFile)
  }

  def getSMLDocument(xmlFile: File): NodeSeq = {
    val doc = XML.loadFile(xmlFile)
    doc \ "DeviceLog"
  }

  def getRRArray(rrData: String): Seq[Int] = {
    val rrArray = rrData.split(" ")
    for (rr <- rrArray) yield rr.toInt
  }

  def parseHeader(header: Node): Try[Header] = {

    //val moveType = Util.getChildElementValue(header, "ActivityType").toInt
    Try {
      val distance = (header \ "Distance")(0).text.toInt
      if (distance == 0) {
        throw new UnsupportedOperationException("Zero distance")
      }
      val dateTime = (header \ "DateTime")(0).text
      Header(
        startTime = timeToUTC(ZonedDateTime.parse(dateTime, dateFormatNoZone)),
        durationMs = ((header \ "Duration")(0).text.toDouble * 1000).toInt,
        calories = Try(Util.kiloCaloriesFromKilojoules((header \ "Energy")(0).text.toDouble)).getOrElse(0),
        distance = distance
      )
    }
  }


  def parseSamples(header: Header, samples: NodeSeq, rr: Seq[Int]): Move = {
    val sampleList = samples \ "Sample"

    class PauseState {
      var pausedTime: Double = 0.0
      var pauseStartTime: Double = 0.0
      var inPause: Boolean = false

      def trackPause(sample: Node): Unit = {
        val pauseTry = sample \ "Events" \ "Pause" \ "State"
        for (pause <- pauseTry) {
          val time = (sample \ "Time").text.toDouble
          if (pause(0).text.equalsIgnoreCase("false")) {
            if (inPause) {
              pausedTime += time - pauseStartTime
              inPause = false
            }
          } else if (pause(0).text.equalsIgnoreCase("true")) {
            pauseStartTime = time
            inPause = true
          }
        }
      }
    }


    val trackPoints = {
      val paused = new PauseState
      sampleList.flatMap { sample =>
        paused.trackPause(sample)
        if (!paused.inPause) {
          // GPS Track POD samples contain no "SampleType" children
          val parseSample = Try {
            val lat = (sample \ "Latitude")(0).text.toDouble * XMLParser.PositionConstant
            val lon = (sample \ "Longitude")(0).text.toDouble * XMLParser.PositionConstant
            val elevation = Try((sample \ "GPSAltitude")(0).text.toInt).toOption
            val utcStr = (sample \ "UTC")(0).text
            val utc = ZonedDateTime.parse(utcStr, dateFormat)
            utc -> GPSPoint(lat, lon, elevation)
          }

          parseSample.toOption
        } else None
      }
    }

    val periodicSamples = {
      val paused = new PauseState
      sampleList.flatMap { sample =>
        paused.trackPause(sample)
        if (!paused.inPause) {
          val periodicSample = for {
            sampleType <- Try((sample \ "SampleType")(0).text) if sampleType.equalsIgnoreCase("periodic")
            distanceStr <- Try((sample \ "Distance")(0).text)
            timeStr <- Try((sample \ "Time")(0).text)
            time = timeStr.toDouble - paused.pausedTime
          } yield {
            val hrTry = Try((sample \ "HR")(0).text)
            val elevationTry = Try((sample \ "Altitude")(0).text)
            val timeTry = Try(ZonedDateTime.parse((sample \ "UTC")(0).text, dateFormat))
            // prefer UTC when present
            val timeUtc = timeTry.getOrElse(header.startTime.plusNanos((time * 1000000L).toLong))
            // replace missing values with zeroes - this is what Quest is recording on failure anyway
            val hr = hrTry.map(_.toInt).getOrElse(0)
            val elevation = elevationTry.map(_.toInt).toOption
            (timeUtc, distanceStr.toDouble, hr, elevation)
          }
          periodicSample.toOption
        } else None
      }
    }

    object Unzipped4 {
      def unapply[A, B, C, D](ts: Seq[(A, B, C, D)]): Some[(Seq[A], Seq[B], Seq[C], Seq[D])] =
        Some((ts.map(_._1), ts.map(_._2), ts.map(_._3), ts.map(_._4)))

    }

    // ignore elevation: let Strava compute it
    val Unzipped4(timeSeq, distanceSeq, hrSeq, _) = periodicSamples

    val hrDistStream = if (hrSeq.size == distanceSeq.size && hrSeq.exists(_ != 0)) {
      val hrWithDist = (hrSeq zip distanceSeq).map { case (hr,d) => HRPoint(hr, d) }
      new DataStreamHRWithDist(header.startTime, header.durationMs, SortedMap(timeSeq zip hrWithDist:_*))
    } else {
      new DataStreamDist(header.startTime, header.durationMs, SortedMap(timeSeq zip distanceSeq:_*))
    }

    val gpsStream = new DataStreamGPS(header.startTime, header.durationMs, SortedMap(trackPoints:_*))

    new Move(header, gpsStream, hrDistStream)
  }

  def parse(xmlFile: File): Try[Move] = {
    XMLParser.log.debug("Parsing " + xmlFile.getName)

    val (doc, header) = if (xmlFile.getName.endsWith(".xml")) {
      val d = getXMLDocument(xmlFile)
      d -> (d \ "Header")(0)
    } else if (xmlFile.getName.endsWith(".sml")) {
      val d = getSMLDocument(xmlFile)
      val dh = d \ "Header"
      d -> dh(0)
    } else throw new UnsupportedOperationException(s"Unknown data format ${xmlFile.getName}")

    val samples = doc \ "Samples"
    val rrData = Try((doc \ "R-R" \ "Data")(0))
    val rr = rrData.map(node => getRRArray(node.text))
    val moves = for {
      h <- parseHeader(header)
    } yield {
      parseSamples(h, samples, rr.getOrElse(Seq()))
    }

    moves
  }

}