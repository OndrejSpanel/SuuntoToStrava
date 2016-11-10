package net.suunto3rdparty
package moveslink

import java.io.File

import org.joda.time.{DateTime=>ZonedDateTime, _}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import java.util.regex.Pattern

import scala.xml._
import org.apache.log4j.Logger
import Util._

import scala.collection.immutable.SortedMap
import scala.util.Try

object XMLParser {
  private val log = Logger.getLogger(XMLParser.getClass)
  private val dateFormat = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss").withZone(DateTimeZone.getDefault)

  def parseSamples(fileName: String, header: Header, samples: Node): Move = {
    val distanceStr = (samples \ "Distance")(0).text
    val heartRateStr = (samples \ "HR")(0).text
    def insertZeroHead(strs: Seq[String]) = {
      if (strs.head.isEmpty) "0" +: strs.tail
      else strs
    }
    def duplicateHead(strs: Seq[String]) = {
      if (strs.head.isEmpty) strs.tail.head +: strs.tail
      else strs
    }

    var currentSum: Double = 0
    val distanceSamples = for {
      distance <- insertZeroHead(distanceStr.split(" "))
    } yield {
      currentSum += distance.toDouble
      currentSum
    }
    val heartRateSamples = for {
      heartRate <- duplicateHead(heartRateStr.split(" "))
    } yield {
      heartRate.toInt
    }

    val validatedHR = heartRateSamples.map {
      hr =>
        if (hr > Settings.maxHR) None // TODO: remove neighbouring samples as well
        else Some(hr)
    }

    // drop two samples around each None

    def slidingRepeatHeadTail[T](s: Seq[T], slide: Int) = {
      val prefix = Seq.fill(slide / 2)(s.head)
      val postfix = Seq.fill(slide - 1 - slide / 2)(s.last)
      val slideSource = prefix ++ s ++ postfix
      slideSource.sliding(slide)
    }

    val slide5 = slidingRepeatHeadTail(validatedHR, 5)

    val validatedCleanedHR = slide5.map {
      case s5 if !s5.contains(None) => s5(2)
      case _ => None
    }.toIndexedSeq

    val timeRange = 0 until header.durationMs by 10000

    def timeMs(ms: Int) = header.startTime.plusMillis(ms*1000)

    val hrWithDist = (validatedHR zip distanceSamples).map{ case (hr, d) => hr.map(s => HRPoint(s, d))}

    val timedMap = (timeRange zip hrWithDist).collect { case (t, Some(s)) =>
      timeMs(t) -> s
    }

    // TODO: laps

    val hrStream = new DataStreamHRWithDist(SortedMap(timedMap:_*))
    new Move(Set(fileName), header.moveHeader, hrStream)
  }

  def parseHeader(headerStr: Node, deviceName: Option[String]) = {

    val durationPattern = Pattern.compile("(\\d+):(\\d+):(\\d+)\\.?(\\d*)")

    val calories = (headerStr \ "Calories")(0).text.toInt
    val distance = (headerStr \ "Distance")(0).text.toInt

    val sportType = Try((headerStr \ "Activity")(0).text.toInt).getOrElse(0)

    import MoveHeader.ActivityType._
    // TODO: add at least most common sports
    val activityType = sportType match {
      case 82 => RunningTrail
      case 75 => Orienteering
      case 5 => MountainBike
      case _ => Unknown
    }

    val timeText = (headerStr \ "Time") (0).text
    val startTime = parseTime(timeText)
    val durationStr = (headerStr \ "Duration")(0).text
    val matcher = durationPattern.matcher(durationStr)
    val duration = if (matcher.matches) {
      val hour = matcher.group(1).toInt
      val minute = matcher.group(2).toInt
      val second = matcher.group(3).toInt
      val ms = if (!matcher.group(4).isEmpty) matcher.group(4).toInt else 0
      (hour * 3600 + minute * 60 + second) * 1000 + ms
    } else 0
    Header(MoveHeader(deviceName.toSet, activityType), startTime, duration, calories, distance)
  }

  def parseTime(timeText: String): ZonedDateTime = {
    timeToUTC(ZonedDateTime.parse(timeText, dateFormat))
  }

  def parse(fileName: String, xmlFile: File): Seq[Move] = {
    XMLParser.log.debug("Parsing " + xmlFile.getName)
    val document = XML.loadFile(xmlFile)

    parseXML(fileName, document)
  }

  def parseXML(fileName: String, document: Elem): Seq[Move] = {

    val deviceNodes = document \ "Device" \ "FullName"

    val deviceName = deviceNodes.headOption.map(_.text)

    val moves = document \ "Moves"

    val moveList = moves \ "Move"
    XMLParser.log.debug(moveList.size + " move elements in this file")
    val suuntoMoves = moveList.zipWithIndex.flatMap { case (moveItem, i) =>
      try {
        val headerNode = (moveItem \ "Header")(0)
        val samples = (moveItem \ "Samples")(0)
        val header = parseHeader(headerNode, deviceName)

        def parseDuration(timeStr: String): Duration = {
          val relTime = LocalTime.parse(timeStr, DateTimeFormat.fullDateTime)
          val relTimeDuration = Seconds.secondsBetween(LocalTime.MIDNIGHT, relTime)
          new Duration(relTimeDuration)
        }

        val lapDurations = for {
          mark <- moveItem \ "Marks" \ "Mark"
          lapDuration <- Try (parseDuration((mark \ "Time")(0).text)).toOption
        } yield {
          lapDuration
        }

        val laps = lapDurations.scanLeft(header.startTime) { (time, duration) => time.plus(duration)}

        val suuntoMove = parseSamples(fileName, header, samples)

        val moveWithLaps = if (laps.nonEmpty) {
          suuntoMove.addStream(suuntoMove, new DataStreamLap(SortedMap(laps.map(time => time -> "Manual"): _*)))
        } else suuntoMove
        Some(moveWithLaps)
      }
      catch {
        case ex: Exception =>
          XMLParser.log.info(s"Data invalid in the no. ${i + 1} of the moves")
          //println(ex.printStackTrace)
          None
      }
    }
    suuntoMoves
  }

}
