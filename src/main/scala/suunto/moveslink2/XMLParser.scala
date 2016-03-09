package com.oldhu.suunto2nike.suunto.moveslink2

import java.io.BufferedReader
import java.io.ByteArrayInputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileReader
import java.io.InputStream
import java.text.ParseException
import java.text.SimpleDateFormat
import java.util.ArrayList
import javax.xml.parsers.DocumentBuilder
import javax.xml.parsers.DocumentBuilderFactory
import org.apache.commons.math.ArgumentOutsideDomainException
import org.apache.commons.math.analysis.interpolation.SplineInterpolator
import org.apache.commons.math.analysis.polynomials.PolynomialSplineFunction
import org.apache.log4j.Logger
import org.w3c.dom.Document
import org.w3c.dom.Element
import org.w3c.dom.NodeList
import com.oldhu.suunto2nike.Util
import com.oldhu.suunto2nike.suunto.SuuntoMove

object XMLParser {
  private val log: Logger = Logger.getLogger(classOf[XMLParser])
  private val dateFormat: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
  private val PositionConstant: Double = 57.2957795131
}

class XMLParser @throws[Exception]
(val xmlFile: File) {
  XMLParser.log.debug("Parsing " + xmlFile.getName)
  var doc: Document = null
  var header: Element = null
  if (xmlFile.getName.endsWith(".xml")) {
    doc = getXMLDocument(xmlFile)
    header = doc.getElementsByTagName("header").item(0).asInstanceOf[Element]
  }
  if (xmlFile.getName.endsWith(".sml")) {
    doc = getSMLDocument(xmlFile)
    header = doc.getElementsByTagName("Header").item(0).asInstanceOf[Element]
  }
  val samples: Element = doc.getElementsByTagName("Samples").item(0).asInstanceOf[Element]
  val rrData: String = Util.getChildElementValue(doc.getDocumentElement, "R-R", "Data")
  val rr: util.ArrayList[Integer] = getRRArray(rrData)
  if (pareseHeader(header)) {
    parseSamples(samples, rr)
  }
  private val suuntoMove: SuuntoMove = new SuuntoMove
  private var parseCompleted: Boolean = false
  def isParseCompleted: Boolean = {
    return parseCompleted
  }
  def getSuuntoMove: SuuntoMove = {
    return suuntoMove
  }
  @throws[Exception]
  private def getXMLDocument(xmlFile: File): Document = {
    val in: BufferedReader = new BufferedReader(new FileReader(xmlFile))
    val firstLine: String = in.readLine
    if (!firstLine.trim.toLowerCase == "<?xml version=\"1.0\" encoding=\"utf-8\"?>") {
      in.close
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
    in.close
    sb.append("</data>")
    val stream: InputStream = new ByteArrayInputStream(sb.toString.getBytes("UTF-8"))
    val dbFactory: DocumentBuilderFactory = DocumentBuilderFactory.newInstance
    val dBuilder: DocumentBuilder = dbFactory.newDocumentBuilder
    return dBuilder.parse(stream)
  }
  @throws[Exception]
  private def getSMLDocument(xmlFile: File): Document = {
    val dbFactory: DocumentBuilderFactory = DocumentBuilderFactory.newInstance
    val dBuilder: DocumentBuilder = dbFactory.newDocumentBuilder
    return dBuilder.parse(new FileInputStream(xmlFile))
  }
  @throws[ParseException]
  private def pareseHeader(header: Element): Boolean = {
    val moveType: Int = Util.getChildElementValue(header, "ActivityType").toInt
    if (moveType != 3 && moveType != 93 && moveType != 82) {
      XMLParser.log.info("    not a running move")
      return false
    }
    val distance: Int = Util.getChildElementValue(header, "Distance").toInt
    if (distance == 0) {
      XMLParser.log.info("    distance zero")
      return false
    }
    suuntoMove.setDistance(distance)
    suuntoMove.setDuration((Util.doubleFromString(Util.getChildElementValue(header, "Duration")).doubleValue * 1000).asInstanceOf[Int])
    suuntoMove.setCalories(Util.kiloCaloriesFromKilojoules(Util.doubleFromString(Util.getChildElementValue(header, "Energy"))))
    val dateTime: String = Util.getChildElementValue(header, "DateTime")
    suuntoMove.setStartTime(XMLParser.dateFormat.parse(dateTime))
    return true
  }
  private def getRRArray(rrData: String): util.ArrayList[Integer] = {
    if (rrData == null) return null
    val rrArray: Array[String] = rrData.split(" ")
    val result: util.ArrayList[Integer] = new util.ArrayList[Integer](rrArray.length)
    for (rr <- rrArray) {
      result.add(rr.toInt)
    }
    return result
  }
  @throws[ArgumentOutsideDomainException]
  private def parseSamples(samples: Element, rr: util.ArrayList[Integer]): Boolean = {
    val sampleList: NodeList = samples.getElementsByTagName("Sample")
    val timeList: util.ArrayList[Double] = new util.ArrayList[Double]
    val hrList: util.ArrayList[Double] = new util.ArrayList[Double]
    val distanceList: util.ArrayList[Double] = new util.ArrayList[Double]
    var pausedTime: Double = 0.0
    var pauseStartTime: Double = 0.0
    var inPause: Boolean = true
    var hasHR: Boolean = false
    var currentAltitude: Int = 0
    var i: Int = 0
    while (i < sampleList.getLength) {
      {
        val sample: Element = sampleList.item(i).asInstanceOf[Element]
        val pause: String = Util.getChildElementValue(sample, "Events", "Pause", "State")
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
        if (inPause) continue //todo: continue is not supported
      val sampleType: String = Util.getChildElementValue(sample, "SampleType")
        if (sampleType == null) continue //todo: continue is not supported
        if (sampleType.equalsIgnoreCase("periodic")) {
          val distanceStr: String = Util.getChildElementValue(sample, "Distance")
          if (distanceStr != null) {
            timeList.add(Util.doubleFromString(Util.getChildElementValue(sample, "Time")) - pausedTime)
            val hrStr: String = Util.getChildElementValue(sample, "HR")
            if (hrStr != null) {
              hasHR = true
            }
            hrList.add(Util.doubleFromString(hrStr))
            val ele: Int = Util.doubleFromString(Util.getChildElementValue(sample, "Altitude")).intValue
            if (ele > 0) {
              currentAltitude = ele
            }
            distanceList.add(Util.doubleFromString(distanceStr))
          }
          continue //todo: continue is not supported
        }
        if (sampleType.toLowerCase.contains("gps")) {
          val lat: Double = Util.doubleFromString(Util.getChildElementValue(sample, "Latitude")) * XMLParser.PositionConstant
          val lon: Double = Util.doubleFromString(Util.getChildElementValue(sample, "Longitude")) * XMLParser.PositionConstant
          var ele: Int = Util.doubleFromString(Util.getChildElementValue(sample, "GPSAltitude")).intValue
          if (ele == 0) {
            ele = currentAltitude
          }
          val utc: String = Util.getChildElementValue(sample, "UTC")
          suuntoMove.addTrackPoint(lat, lon, ele, utc)
        }
      }
      ({i += 1; i})
    }
    var timeArray: Array[Double] = new Array[Double](timeList.size)
    var hrArray: Array[Double] = new Array[Double](hrList.size)
    val distanceArray: Array[Double] = new Array[Double](distanceList.size)
    populateTimeArray(timeArray, timeList)
    populateHRArray(hrArray, hrList, timeArray)
    populateDistanceArray(distanceArray, distanceList)
    val timeToDistance: Nothing = generateTimeToDistanceSplineFunction(timeArray, distanceArray)
    var timeToHR: Nothing = null
    if (hasHR || rr == null) {
      timeToHR = generateTimeToHRSplineFunction(timeArray, hrArray)
    }
    else {
      timeArray = new Array[Double](rr.size)
      hrArray = new Array[Double](rr.size)
      var time: Double = 0
      var i: Int = 0
      while (i < rr.size) {
        {
          val value: Int = rr.get(i)
          time += value
          timeArray(i) = time
          hrArray(i) = 60000.0 / value
        }
        ({i += 1; i})
      }
      timeToHR = generateTimeToHRSplineFunction(timeArray, hrArray)
    }
    var t: Double = 0
    while (t < suuntoMove.getDuration) {
      {
        t += 10 * 1000
        val hr: Int = interpolate(timeToHR, t).toInt
        val distance: Int = interpolate(timeToDistance, t).toInt
        suuntoMove.addHeartRateSample(hr)
        suuntoMove.addDistanceSample(distance)
      }
    }
    parseCompleted = true
    return true
  }
  @throws[ArgumentOutsideDomainException]
  private def interpolate(spline: Nothing, x: Double): Double = {
    try {
      return spline.value(x)
    }
    catch {
      case aode: Nothing => {
        val knots: Array[Double] = spline.getKnots
        return spline.value(knots(if ((x < knots(0))) 0
        else spline.getN - 1))
      }
    }
  }
  private def generateTimeToDistanceSplineFunction(timeArray: Array[Double], distanceArray: Array[Double]): Nothing = {
    val interpolator: Nothing = new Nothing
    return interpolator.interpolate(timeArray, distanceArray)
  }
  private def populateDistanceArray(distanceArray: Array[Double], distanceList: util.ArrayList[Double]) {
    var i: Int = 0
    while (i < distanceList.size) {
      {
        distanceArray(i) = distanceList.get(i).doubleValue
      }
      ({i += 1; i})
    }
  }
  private def populateHRArray(hrArray: Array[Double], hrList: util.ArrayList[Double], timeArray: Array[Double]) {
    var i: Int = 0
    while (i < hrList.size) {
      {
        hrArray(i) = hrList.get(i).doubleValue * 60
      }
      ({i += 1; i})
    }
  }
  private def populateTimeArray(timeArray: Array[Double], timeList: util.ArrayList[Double]) {
    var i: Int = 0
    while (i < timeList.size) {
      {
        timeArray(i) = timeList.get(i).doubleValue * 1000
      }
      ({i += 1; i})
    }
  }
  private def generateTimeToHRSplineFunction(timeArray: Array[Double], hrArray: Array[Double]): Nothing = {
    val interpolator: Nothing = new Nothing
    return interpolator.interpolate(timeArray, hrArray)
  }
}