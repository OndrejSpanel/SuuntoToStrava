package net.suunto3rdparty
package moveslink

import java.io.File
import java.time.{ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.regex.Pattern

import scala.xml._
import org.apache.log4j.Logger
import Util._

import scala.collection.immutable.SortedMap

object XMLParser {
  private val log = Logger.getLogger(XMLParser.getClass)

  def parseSamples(header: Header, samples: Node): Move = {
    val distanceStr = (samples \ "Distance")(0).text
    val heartRateStr = (samples \ "HR")(0).text
    var currentSum: Double = 0
    val distanceSamples = for {
      distance <- distanceStr.split(" ")
      if !distance.trim.isEmpty
    } yield {
      currentSum += distance.toDouble
      currentSum
    }
    val heartRateSamples = for {
      heartRate <- heartRateStr.split(" ")
      if !heartRate.trim.isEmpty
    } yield {
      heartRate.toInt
    }

    val timeRange = 0 until header.durationMs by 10000

    def timeMs(ms: Int) = header.startTime.plusNanos(ms*1000000L)

    val hrWithDist = (heartRateSamples zip distanceSamples).map{ case (hr, d) => HRPoint(hr, d)}

    val timedMap = (timeRange zip hrWithDist).map { case (t, s) =>
      timeMs(t) -> s
    }

    val hrStream = new DataStreamHRWithDist(SortedMap(timedMap:_*))
    new Move(MoveHeader(), hrStream)
  }

  def parseHeader(headerStr: Node) = {
    val dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneId.systemDefault)

    val durationPattern = Pattern.compile("(\\d+):(\\d+):(\\d+)\\.?(\\d*)")

    val calories = (headerStr \ "Calories")(0).text.toInt
    val distance = (headerStr \ "Distance")(0).text.toInt

    val timeText = (headerStr \ "Time") (0).text
    val startTime = timeToUTC(ZonedDateTime.parse(timeText, dateFormat))
    val durationStr = (headerStr \ "Duration")(0).text
    val matcher = durationPattern.matcher(durationStr)
    val duration = if (matcher.matches) {
      val hour = matcher.group(1).toInt
      val minute = matcher.group(2).toInt
      val second = matcher.group(3).toInt
      val ms = if (!matcher.group(4).isEmpty) matcher.group(4).toInt else 0
      (hour * 3600 + minute * 60 + second) * 1000 + ms
    } else 0
    Header(startTime, duration, calories, distance)
  }

  def parse(xmlFile: File): Seq[Move] = {
    XMLParser.log.debug("Parsing " + xmlFile.getName)
    val document = XML.loadFile(xmlFile)
    val moves = document \ "Moves"

    val moveList = moves \ "Move"
    XMLParser.log.debug(moveList.size + " move elements in this file")
    val suuntoMoves = moveList.zipWithIndex.flatMap { case (moveItem, i) =>
      try {
        val headerStr = (moveItem \ "Header")(0)
        val samples = (moveItem \ "Samples")(0)
        val header = parseHeader(headerStr)
        val suuntoMove = parseSamples(header, samples)
        Some(suuntoMove)
      }
      catch {
        case e: Exception =>
          XMLParser.log.info(s"Data invalid in the no. ${i + 1} of the moves")
          None
      }
    }
    suuntoMoves
  }

}
