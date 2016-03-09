package net.suunto3rdparty
package moveslink

import java.io.File
import java.util.regex.{Matcher, Pattern}
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import NodeListOps._

import org.apache.log4j.Logger
import org.w3c.dom.{Document, Element, NodeList}

object XMLParser {private val log: Logger = Logger.getLogger(classOf[XMLParser])}

class XMLParser(var xmlFile: File) {
  XMLParser.log.debug("Parsing " + xmlFile.getName)
  private val dbFactory = DocumentBuilderFactory.newInstance
  private val dBuilder = dbFactory.newDocumentBuilder
  private val document = dBuilder.parse(xmlFile)
  private val movesCountList = document.getElementsByTagName("MovesCount")
  if (movesCountList.getLength != 1) {
    throw new Exception("not valid moves count xml file, MovesCount node count: " + movesCountList.getLength)
  }
  private val movesCount = movesCountList.item(0).asInstanceOf[Element]
  private val movesList = movesCount.getElementsByTagName("Moves")
  if (movesList.getLength != 1) {
    throw new Exception("not valid moves count xml file, Moves node count: " + movesList.getLength)
  }
  private val moves = movesList.item(0).asInstanceOf[Element]
  final private val durationPattern: Pattern = Pattern.compile("(\\d+):(\\d+):(\\d+)\\.?(\\d*)")

  def parse: Seq[SuuntoMove] = {
    val moveList = moves.getElementsByTagName("Move")
    if (moveList.getLength == 0) {
      XMLParser.log.debug("No moves data in " + xmlFile.getName)
      return null
    }
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

  private def parseSamples(samples: Element): SuuntoMove = {
    val suuntoMove = new SuuntoMove
    val distanceStr: String = getChildElementValue(samples, "Distance")
    val heartRateStr: String = getChildElementValue(samples, "HR")
    var currentSum: Int = 0
    for (distance <- distanceStr.split(" ")) {
      if (!distance.trim.isEmpty) {
        currentSum += distance.toInt
        suuntoMove.addDistanceSample(currentSum)
      }
    }
    for (heartRate <- heartRateStr.split(" ")) {
      if (!heartRate.trim.isEmpty) {
        suuntoMove.addHeartRateSample(heartRate.toInt)
      }
    }
    suuntoMove
  }

  private def parseHeader(header: Element, suuntoMove: SuuntoMove) {
    suuntoMove.setCalories(getChildElementValue(header, "Calories").toInt)
    suuntoMove.setDistance(getChildElementValue(header, "Distance").toInt)
    suuntoMove.setStartTime(getChildElementValue(header, "Time"))
    val durationStr: String = getChildElementValue(header, "Duration")
    val matcher: Matcher = durationPattern.matcher(durationStr)
    if (matcher.matches) {
      val hour: Int = matcher.group(1).toInt
      val minute: Int = matcher.group(2).toInt
      val second: Int = matcher.group(3).toInt
      var ms: Int = 0
      if (!matcher.group(4).isEmpty) {
        ms = matcher.group(4).toInt
      }
      ms = (hour * 3600 + minute * 60 + second) * 1000 + ms
      suuntoMove.setDuration(ms)
    }
  }
  private def getChildElementValue(parent: Element, elementName: String): String = {
    val child = parent.getElementsByTagName(elementName).item(0).asInstanceOf[Element]
    child.getTextContent
  }
}