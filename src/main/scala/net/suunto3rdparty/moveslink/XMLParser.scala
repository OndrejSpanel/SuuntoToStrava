package net.suunto3rdparty
package moveslink

import java.io.File
import java.util.regex.Pattern
import javax.xml.parsers.DocumentBuilderFactory
import NodeListOps._

import org.apache.log4j.Logger
import org.w3c.dom.Element

object XMLParser {
  private val log = Logger.getLogger(XMLParser.getClass)

  def parseSamples(samples: Element): SuuntoMove = {
    val distanceStr = getChildElementValue(samples, "Distance")
    val heartRateStr = getChildElementValue(samples, "HR")
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

  def parseHeader(header: Element, suuntoMove: SuuntoMove) = {
    val durationPattern = Pattern.compile("(\\d+):(\\d+):(\\d+)\\.?(\\d*)")

    suuntoMove.calories = getChildElementValue(header, "Calories").toInt
    suuntoMove.distance = getChildElementValue(header, "Distance").toInt
    suuntoMove.startTime = getChildElementValue(header, "Time")
    val durationStr = getChildElementValue(header, "Duration")
    val matcher = durationPattern.matcher(durationStr)
    if (matcher.matches) {
      val hour = matcher.group(1).toInt
      val minute = matcher.group(2).toInt
      val second = matcher.group(3).toInt
      val ms = if (!matcher.group(4).isEmpty) matcher.group(4).toInt else 0
      suuntoMove.duration = (hour * 3600 + minute * 60 + second) * 1000 + ms
    }
  }
  def getChildElementValue(parent: Element, elementName: String): String = {
    val child = parent.getElementsByTagName(elementName).item(0).asInstanceOf[Element]
    child.getTextContent
  }

  def parse(xmlFile: File): Seq[SuuntoMove] = {
    XMLParser.log.debug("Parsing " + xmlFile.getName)
    val dbFactory = DocumentBuilderFactory.newInstance
    val dBuilder = dbFactory.newDocumentBuilder
    val document = dBuilder.parse(xmlFile)
    val movesCountList = document.getElementsByTagName("MovesCount")
    if (movesCountList.getLength != 1) {
      throw new Exception("not valid moves count xml file, MovesCount node count: " + movesCountList.getLength)
    }
    val movesCount = movesCountList.item(0).asInstanceOf[Element]
    val movesList = movesCount.getElementsByTagName("Moves")
    if (movesList.getLength != 1) {
      throw new Exception("not valid moves count xml file, Moves node count: " + movesList.getLength)
    }
    val moves = movesList.item(0).asInstanceOf[Element]


    val moveList = moves.getElementsByTagName("Move")
    XMLParser.log.debug(moveList.getLength + " move elements in this file")
    val suuntoMoves = moveList.toSeq.zipWithIndex.flatMap { case (moveItem, i) =>
      try {
        val move = moveItem.asInstanceOf[Element]
        val header = move.getElementsByTagName("Header").item(0).asInstanceOf[Element]
        val samples = move.getElementsByTagName("Samples").item(0).asInstanceOf[Element]
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
