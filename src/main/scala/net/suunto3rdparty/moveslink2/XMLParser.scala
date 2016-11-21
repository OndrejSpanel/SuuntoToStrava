package net.suunto3rdparty
package moveslink2

import java.io._

import org.joda.time.{DateTimeZone, DateTime => ZonedDateTime}
import org.joda.time.format.{DateTimeFormat, ISODateTimeFormat}
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

  private val dateFormat = ISODateTimeFormat.dateTimeNoMillis
  private val dateFormatNoZone = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss").withZone(DateTimeZone.getDefault)

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

  def getDeviceLog(doc: Elem): Node = (doc \ "DeviceLog") (0)

  def getXMLDocument(xmlFile: File): Elem = {
    XML.loadFile(xmlFile)
  }

  def getSMLDocument(xmlFile: File): Node = {
    val doc = XML.loadFile(xmlFile)
    getDeviceLog(doc)
  }


  def getRRArray(rrData: String): Seq[Int] = {
    val rrArray = rrData.split(" ")
    for (rr <- rrArray) yield rr.toInt
  }

  def parseHeader(doc: Node): Try[Header] = {
    val header = doc \ "Header"

    val deviceName = (doc \ "Device" \ "Name").headOption.map(_.text)
    //val moveType = Util.getChildElementValue(header, "ActivityType").toInt
    Try {
      val distance = (header \ "Distance")(0).text.toInt
      if (distance == 0) {
        throw new UnsupportedOperationException("Zero distance")
      }
      val dateTime = (header \ "DateTime")(0).text
      Header(
        MoveHeader(deviceName.toSet, MoveHeader.ActivityType.Unknown),
        startTime = timeToUTC(ZonedDateTime.parse(dateTime, dateFormatNoZone)),
        durationMs = ((header \ "Duration")(0).text.toDouble * 1000).toInt,
        calories = Try(Util.kiloCaloriesFromKilojoules((header \ "Energy")(0).text.toDouble)).getOrElse(0),
        distance = distance
      )
    }
  }


  def parseSamples(fileName: String, header: Header, samples: NodeSeq, rr: Seq[Int]): Move = {
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

    val lapPoints = {
      /* GPS Track Pod lap is stored as:
			<Sample>
				<UTC>2016-03-18T10:19:31</UTC>
				<Time>24.891</Time>
      	<Events>
					<Lap>
						<Type>Manual</Type>
						<Duration>24.9</Duration>
						<Distance>15</Distance>
					</Lap>
				</Events>
			</Sample>
      */
      sampleList.toList.flatMap { sample =>
        val lapTime = Try {
          //noinspection ScalaUnusedSymbol
          for (lap <- (sample \ "Events" \ "Lap" \ "Type").headOption) yield {
            val timestamp = (sample \ "UTC") (0).text
            val utc = timeToUTC(ZonedDateTime.parse(timestamp, dateFormatNoZone))
            Lap(lap.text, utc)
          }
        }

        lapTime.toOption.flatten.toSeq
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
            val timeTry = Try(ZonedDateTime.parse((sample \ "UTC")(0).text))
            val timeSim = header.startTime.plusMillis((time * 1000).toInt)
            // prefer UTC when present
            val timeUtc = timeTry.getOrElse(timeSim)
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
      new DataStreamHRWithDist(SortedMap(timeSeq zip hrWithDist:_*))
    } else {
      new DataStreamDist(SortedMap(timeSeq zip distanceSeq:_*))
    }

    val gpsStream = new DataStreamGPS(SortedMap(trackPoints:_*))

    val gpsMove = if (lapPoints.nonEmpty) {
      val lapStream = new DataStreamLap(SortedMap(lapPoints.map(l => l.timestamp -> l.name):_*))
      new Move(Set(fileName), header.moveHeader, gpsStream, hrDistStream, lapStream)
    } else {
      new Move(Set(fileName), header.moveHeader, gpsStream, hrDistStream)
    }


    val gpsDroppedEmpty = gpsStream.dropAlmostEmpty match {
      case Some((keepStart, keepEnd)) =>
        gpsMove.span(keepStart)._2.flatMap(_.span(keepEnd)._1)
      case None =>
        None
    }
    gpsDroppedEmpty.getOrElse(new Move(Set(fileName), header.moveHeader))
  }

  def parse(fileName: String, xmlFile: File): Try[Move] = {
    XMLParser.log.debug("Parsing " + xmlFile.getName)

    val doc = if (xmlFile.getName.endsWith(".xml")) {
      getXMLDocument(xmlFile)
    } else if (xmlFile.getName.endsWith(".sml")) {
      getSMLDocument(xmlFile)
    } else throw new UnsupportedOperationException(s"Unknown data format ${xmlFile.getName}")
    parseXML(fileName, doc)
  }

  def parseXML(fileName: String, doc: Node): Try[Move] = {
    val samples = doc \ "Samples"
    val rrData = Try((doc \ "R-R" \ "Data")(0))
    val rr = rrData.map(node => getRRArray(node.text))
    val moves = for {
      h <- parseHeader(doc)
    } yield {
      parseSamples(fileName, h, samples, rr.getOrElse(Seq()))
    }

    moves
  }

}