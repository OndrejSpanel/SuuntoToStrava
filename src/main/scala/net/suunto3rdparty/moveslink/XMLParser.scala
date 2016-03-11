package net.suunto3rdparty
package moveslink

import java.io.File
import java.time.ZonedDateTime
import java.util.regex.Pattern

import scala.xml._
import org.apache.log4j.Logger

object XMLParser {
  private val log = Logger.getLogger(XMLParser.getClass)

  def parseSamples(samples: Node): SuuntoMove = {
    val distanceStr = (samples \ "Distance")(0).text
    val heartRateStr = (samples \ "HR")(0).text
    var currentSum: Int = 0
    val distanceSamples = for {
      distance <- distanceStr.split(" ")
      if !distance.trim.isEmpty
    } yield {
      currentSum += distance.toInt
      currentSum
    }
    val heartRateSamples = for {
      heartRate <- heartRateStr.split(" ")
      if !heartRate.trim.isEmpty
    } yield {
      heartRate.toInt
    }
    new SuuntoMove(distanceSamples, heartRateSamples)
  }

  def parseHeader(header: Node, suuntoMove: SuuntoMove) = {
    val durationPattern = Pattern.compile("(\\d+):(\\d+):(\\d+)\\.?(\\d*)")

    suuntoMove.calories = (header \ "Calories")(0).text.toInt
    suuntoMove.distance = (header \ "Distance")(0).text.toInt

    val timeText = (header \ "Time") (0).text
    suuntoMove.startTime = ZonedDateTime.parse(timeText, Util.dateFormat)
    val durationStr = (header \ "Duration")(0).text
    val matcher = durationPattern.matcher(durationStr)
    if (matcher.matches) {
      val hour = matcher.group(1).toInt
      val minute = matcher.group(2).toInt
      val second = matcher.group(3).toInt
      val ms = if (!matcher.group(4).isEmpty) matcher.group(4).toInt else 0
      suuntoMove.duration = (hour * 3600 + minute * 60 + second) * 1000 + ms
    }
  }

  def parse(xmlFile: File): Seq[SuuntoMove] = {
    XMLParser.log.debug("Parsing " + xmlFile.getName)
    val document = XML.loadFile(xmlFile)
    val moves = document \ "Moves"

    val moveList = moves \ "Move"
    XMLParser.log.debug(moveList.size + " move elements in this file")
    val suuntoMoves = moveList.zipWithIndex.flatMap { case (moveItem, i) =>
      try {
        val header = (moveItem \ "Header")(0)
        val samples = (moveItem \ "Samples")(0)
        val suuntoMove = parseSamples(samples)
        parseHeader(header, suuntoMove)
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
