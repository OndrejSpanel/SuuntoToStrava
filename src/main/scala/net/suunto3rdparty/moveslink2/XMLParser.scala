package net.suunto3rdparty
package moveslink2

import java.io._
import java.text.SimpleDateFormat
import javax.xml.parsers.DocumentBuilderFactory

import NodeListOps._
import org.apache.commons.math.ArgumentOutsideDomainException
import org.apache.commons.math.analysis.interpolation.SplineInterpolator
import org.apache.commons.math.analysis.polynomials.PolynomialSplineFunction
import org.apache.log4j.Logger
import org.w3c.dom.{Document, Element}

import scala.util.{Failure, Success, Try}

object XMLParser {
  private val log = Logger.getLogger(XMLParser.getClass)
  private val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
  private val PositionConstant = 57.2957795131

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
  def populateTimeArray(timeList: Seq[Double]): Seq[Double] = {
    for (time <- timeList) yield time * 1000
  }
  def generateTimeToHRSplineFunction(timeArray: Seq[Double], hrArray: Seq[Double]): PolynomialSplineFunction = {
    val interpolator = new SplineInterpolator
    interpolator.interpolate(timeArray.toArray, hrArray.toArray)
  }

  def getXMLDocument(xmlFile: File): Document = {
    val in = new BufferedReader(new FileReader(xmlFile))
    val firstLine = in.readLine
    if (firstLine.trim.toLowerCase != "<?xml version=\"1.0\" encoding=\"utf-8\"?>") {
      in.close()
      throw new Exception("File format not correct: " + xmlFile.getName)
    }
    val sb = new StringBuilder
    sb.append(firstLine)
    sb.append("<data>")
    while (in.ready) {
      sb.append(in.readLine)
    }
    in.close()
    sb.append("</data>")
    val stream = new ByteArrayInputStream(sb.toString.getBytes("UTF-8"))
    val dbFactory = DocumentBuilderFactory.newInstance
    val dBuilder = dbFactory.newDocumentBuilder
    dBuilder.parse(stream)
  }

  def getSMLDocument(xmlFile: File): Document = {
    val dbFactory = DocumentBuilderFactory.newInstance
    val dBuilder = dbFactory.newDocumentBuilder
    dBuilder.parse(new FileInputStream(xmlFile))
  }

  def getRRArray(rrData: String): Seq[Int] = {
    val rrArray = rrData.split(" ")
    for (rr <- rrArray) yield rr.toInt
  }

  def parseHeader(header: Element): Try[SuuntoMove.Header] = {
    //val moveType = Util.getChildElementValue(header, "ActivityType").toInt
    val distance = Util.getChildElementValue(header, "Distance").toInt
    if (distance == 0) {
      return Failure(new UnsupportedOperationException("Zero distance"))
    }
    val dateTime = Util.getChildElementValue(header, "DateTime")
    Success(SuuntoMove.Header(
      startTime = SuuntoMove.dateFormat.format(XMLParser.dateFormat.parse(dateTime)),
      duration = (Util.doubleFromString(Util.getChildElementValue(header, "Duration")).doubleValue * 1000).toInt,
      Try(Util.getChildElementValue(header, "Energy")).map(_.toDouble).map(Util.kiloCaloriesFromKilojoules).getOrElse(0),
      distance = distance
    ))
  }


  def parseSamples(header: SuuntoMove.Header, samples: Element, rr: Seq[Int]): SuuntoMove = {
    val sampleList = samples.getElementsByTagName("Sample")

    class PauseState {
      var pausedTime: Double = 0.0
      var pauseStartTime: Double = 0.0
      var inPause: Boolean = false

      def trackPause(sample: Element): Unit = {
        val pauseTry = Try(Util.getChildElementValue(sample, "Events", "Pause", "State"))
        for (pause <- pauseTry) {
          val time = Util.doubleFromString(Util.getChildElementValue(sample, "Time"))
          if (pause.equalsIgnoreCase("false")) {
            if (inPause) {
              pausedTime += time - pauseStartTime
              inPause = false
            }
          } else if (pause.equalsIgnoreCase("true")) {
            pauseStartTime = time
            inPause = true
          }
        }
      }
    }


    val samplesSeq = sampleList.toSeq
    val trackPoints = {
      val paused = new PauseState
      samplesSeq.flatMap { sampleItem =>
        val sample = sampleItem.asInstanceOf[Element]
        paused.trackPause(sample)
        if (!paused.inPause) {
          // GPS Track POD samples contain no "SampleType" children
          val parseSample = Try {
            val lat = Util.getChildElementValue(sample, "Latitude").toDouble * XMLParser.PositionConstant
            val lon = Util.getChildElementValue(sample, "Longitude").toDouble * XMLParser.PositionConstant
            val elevation = Try(Util.getChildElementValue(sample, "GPSAltitude")).map(_.toInt).toOption
            val utc = Util.getChildElementValue(sample, "UTC")
            SuuntoMove.TrackPoint(lat, lon, elevation, utc)
          }

          parseSample.toOption
        } else None
      }
    }

    val periodicSamples = {
      val paused = new PauseState
      samplesSeq.flatMap { sampleItem =>
        val sample = sampleItem.asInstanceOf[Element]
        paused.trackPause(sample)
        if (!paused.inPause) {
          val periodicSample = for {
            sampleType <- Try(Util.getChildElementValue(sample, "SampleType")) if sampleType.equalsIgnoreCase("periodic")
            distanceStr <- Try(Util.getChildElementValue(sample, "Distance"))
            timeStr <- Try(Util.getChildElementValue(sample, "Time"))
            time = Util.doubleFromString(timeStr) - paused.pausedTime
          } yield {
            val hrTry = Try(Util.getChildElementValue(sample, "HR"))
            val elevationTry = Try(Util.getChildElementValue(sample, "Altitude"))
            val hr = hrTry.toOption.map(_.toDouble)
            val elevation = elevationTry.map(_.toInt).toOption
            // TODO: may contain UTC directly - prefer it
            (time, distanceStr.toDouble, hr, elevation)
          }
          periodicSample.toOption
        } else None
      }
    }

    object Unzipped4 {
      def unapply[A, B, C, D](ts: Seq[(A, B, C, D)]): Some[(Seq[A], Seq[B], Seq[C], Seq[D])] =
        Some((ts.map(_._1), ts.map(_._2), ts.map(_._3), ts.map(_._4)))

    }
    val Unzipped4(timeSeq, distanceArray, hrSeq, elevArray) = periodicSamples
    val hasHR = hrSeq.exists(_.nonEmpty)

    val timeArray = populateTimeArray(timeSeq)
    val hrArray = populateHRArray(hrSeq.map(_.getOrElse(0.0)))

    val timeToDistance = generateTimeToDistanceSplineFunction(timeArray, distanceArray)
    val timeToHR = if (hasHR || rr.isEmpty) {
      generateTimeToHRSplineFunction(timeArray, hrArray)
    } else {
      var time: Double = 0
      val timeRRArray = for (value <- rr) yield {
        time += value
        time
      }
      val hrRRArray = for (value <- rr) yield 60000.0 / value

      generateTimeToHRSplineFunction(timeRRArray, hrRRArray)
    }
    val hrAndDistance = for (t <- 0 until header.duration by 10000) yield {
      val hr = interpolate(timeToHR, t).toInt
      val distance = interpolate(timeToDistance, t).toInt
      (hr, distance)
    }

    val (hrRes, distanceRes) = hrAndDistance.unzip

    new SuuntoMove(header, distanceRes, hrRes, trackPoints)
  }

  def parse(xmlFile: File): Try[SuuntoMove] = {
    XMLParser.log.debug("Parsing " + xmlFile.getName)

    val (doc, header) = if (xmlFile.getName.endsWith(".xml")) {
      val d = getXMLDocument(xmlFile)
      d -> d.getElementsByTagName("header").item(0).asInstanceOf[Element]
    }
    else if (xmlFile.getName.endsWith(".sml")) {
      val d = getSMLDocument(xmlFile)
      d -> d.getElementsByTagName("Header").item(0).asInstanceOf[Element]
    }
    else throw new UnsupportedOperationException(s"Unknown data format ${xmlFile.getName}")

    val samples = doc.getElementsByTagName("Samples").item(0).asInstanceOf[Element]
    val rrData = Try(Util.getChildElementValue(doc.getDocumentElement, "R-R", "Data"))
    val rr = rrData.map(getRRArray)
    val moves = for (h <- parseHeader(header)) yield {
      parseSamples(h, samples, rr.getOrElse(Seq()))
    }

    moves
  }

}