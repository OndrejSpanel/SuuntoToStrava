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
import org.w3c.dom.{Document, Element, NodeList}

import scala.collection.mutable

object XMLParser {
  private val log: Logger = Logger.getLogger(classOf[XMLParser])
  private val dateFormat: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
  private val PositionConstant: Double = 57.2957795131
}

class XMLParser(val xmlFile: File) {
  XMLParser.log.debug("Parsing " + xmlFile.getName)

  private val suuntoMove = new SuuntoMove
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
  val rrData = Util.getChildElementValue(doc.getDocumentElement, "R-R", "Data")
  val rr = getRRArray(rrData)
  if (parseHeader(header)) {
    parseSamples(samples, rr)
  }
  private var parseCompleted: Boolean = false
  def isParseCompleted = parseCompleted
  def getSuuntoMove = suuntoMove

  private def getXMLDocument(xmlFile: File): Document = {
    val in: BufferedReader = new BufferedReader(new FileReader(xmlFile))
    val firstLine: String = in.readLine
    if (firstLine.trim.toLowerCase != "<?xml version=\"1.0\" encoding=\"utf-8\"?>") {
      in.close()
      throw new Exception("File format not correct: " + xmlFile.getName)
    }
    val sb: StringBuilder = new StringBuilder
    sb.append(firstLine)
    sb.append("<data>")
    while (in.ready) {
      {
        sb.append(in.readLine)
      }
    }
    in.close()
    sb.append("</data>")
    val stream = new ByteArrayInputStream(sb.toString.getBytes("UTF-8"))
    val dbFactory = DocumentBuilderFactory.newInstance
    val dBuilder = dbFactory.newDocumentBuilder
    dBuilder.parse(stream)
  }

  private def getSMLDocument(xmlFile: File): Document = {
    val dbFactory = DocumentBuilderFactory.newInstance
    val dBuilder = dbFactory.newDocumentBuilder
    dBuilder.parse(new FileInputStream(xmlFile))
  }

  private def parseHeader(header: Element): Boolean = {
    val moveType = Util.getChildElementValue(header, "ActivityType").toInt
    if (moveType != 3 && moveType != 93 && moveType != 82) {
      XMLParser.log.info("    not a running move")
      return false
    }
    val distance = Util.getChildElementValue(header, "Distance").toInt
    if (distance == 0) {
      XMLParser.log.info("    distance zero")
      return false
    }
    suuntoMove.setDistance(distance)
    suuntoMove.setDuration((Util.doubleFromString(Util.getChildElementValue(header, "Duration")).doubleValue * 1000).asInstanceOf[Int])
    suuntoMove.setCalories(Util.kiloCaloriesFromKilojoules(Util.doubleFromString(Util.getChildElementValue(header, "Energy"))))
    val dateTime = Util.getChildElementValue(header, "DateTime")
    suuntoMove.setStartTime(XMLParser.dateFormat.parse(dateTime))
    true
  }
  private def getRRArray(rrData: String): Array[Int] = {
    if (rrData == null) return null
    val rrArray = rrData.split(" ")
    for (rr <- rrArray) yield rr.toInt
  }

  private def parseSamples(samples: Element, rr: Seq[Int]): Boolean = {
    val sampleList: NodeList = samples.getElementsByTagName("Sample")
    val timeList = mutable.ArrayBuffer[Double]()
    val hrList = mutable.ArrayBuffer[Double]()
    val distanceList = mutable.ArrayBuffer[Double]()
    var pausedTime: Double = 0.0
    var pauseStartTime: Double = 0.0
    var inPause: Boolean = false
    var hasHR: Boolean = false
    var currentAltitude: Int = 0
    for (sampleItem <- sampleList.toSeq) {
        val sample = sampleItem.asInstanceOf[Element]
        val pause = Util.getChildElementValue(sample, "Events", "Pause", "State")
        if (pause != null) {
          val time: Double = Util.doubleFromString(Util.getChildElementValue(sample, "Time"))
          if (pause.equalsIgnoreCase("false")) {
            if (inPause) {
              pausedTime += time - pauseStartTime
              inPause = false
            }
          }
          else if (pause.equalsIgnoreCase("true")) {
            pauseStartTime = time
            inPause = true
          }
        }
        if (!inPause) {
          val sampleType = Util.getChildElementValue(sample, "SampleType")
          if (sampleType != null && sampleType.equalsIgnoreCase("periodic")) {
            val distanceStr = Util.getChildElementValue(sample, "Distance")
            if (distanceStr != null) {
              timeList += Util.doubleFromString(Util.getChildElementValue(sample, "Time")) - pausedTime
              val hrStr = Util.getChildElementValue(sample, "HR")
              if (hrStr != null) {
                hasHR = true
              }
              hrList += Util.doubleFromString(hrStr)
              val ele = Util.doubleFromString(Util.getChildElementValue(sample, "Altitude")).intValue
              if (ele > 0) {
                currentAltitude = ele
              }
              distanceList += Util.doubleFromString(distanceStr)
            }
          } else {
            // GPS Track POD samples contain no "SampleType" children
            val lat = Util.doubleFromString(Util.getChildElementValue(sample, "Latitude")) * XMLParser.PositionConstant
            val lon = Util.doubleFromString(Util.getChildElementValue(sample, "Longitude")) * XMLParser.PositionConstant
            var ele = Util.doubleFromString(Util.getChildElementValue(sample, "GPSAltitude")).toInt
            if (ele == 0) {
              ele = currentAltitude
            }
            val utc = Util.getChildElementValue(sample, "UTC")
            suuntoMove.addTrackPoint(lat, lon, ele, utc)
          }
        }
    }
    val timeArray = populateTimeArray(timeList)
    val hrArray = populateHRArray(hrList)
    val distanceArray = populateDistanceArray(distanceList)
    val timeToDistance = generateTimeToDistanceSplineFunction(timeArray, distanceArray)
    val timeToHR = if (hasHR || rr == null) {
      generateTimeToHRSplineFunction(timeArray, hrArray)
    }
    else {
      var time: Double = 0
      val timeArray = for (value <- rr) yield {
        time += value
        time
      }
      val hrArray = for (value <- rr) yield 60000.0 / value

      generateTimeToHRSplineFunction(timeArray, hrArray)
    }
    var t: Double = 0
    while (t < suuntoMove.getDuration) {
      {
        t += 10 * 1000
        val hr = interpolate(timeToHR, t).toInt
        val distance = interpolate(timeToDistance, t).toInt
        suuntoMove.addHeartRateSample(hr)
        suuntoMove.addDistanceSample(distance)
      }
    }
    parseCompleted = true
    true
  }

  private def interpolate(spline: PolynomialSplineFunction, x: Double): Double = {
    try {
      spline.value(x)
    }
    catch {
      case _: ArgumentOutsideDomainException =>
        val knots = spline.getKnots
        spline.value(knots(if (x < knots(0)) 0 else spline.getN - 1))
    }
  }
  private def generateTimeToDistanceSplineFunction(timeArray: Seq[Double], distanceArray: Seq[Double]): PolynomialSplineFunction = {
    val interpolator = new SplineInterpolator
    interpolator.interpolate(timeArray.toArray, distanceArray.toArray)
  }
  private def populateDistanceArray(distanceList: Seq[Double]): Seq[Double] =  {
    for (d <- distanceList) yield d
  }

  private def populateHRArray(hrList: Seq[Double]): Seq[Double] =  {
    for (hr <- hrList) yield hr * 60
  }
  private def populateTimeArray(timeList: Seq[Double]): Seq[Double] = {
    for (time <- timeList) yield time * 1000
  }
  private def generateTimeToHRSplineFunction(timeArray: Seq[Double], hrArray: Seq[Double]): PolynomialSplineFunction = {
    val interpolator = new SplineInterpolator
    interpolator.interpolate(timeArray.toArray, hrArray.toArray)
  }
}