package com.oldhu.suunto2nike.suunto.moveslink

import java.io.File
import java.text.ParseException
import java.util.ArrayList
import java.util.regex.Matcher
import java.util.regex.Pattern
import javax.xml.parsers.DocumentBuilder
import javax.xml.parsers.DocumentBuilderFactory
import org.apache.log4j.Logger
import org.w3c.dom.Document
import org.w3c.dom.Element
import org.w3c.dom.NodeList
import com.oldhu.suunto2nike.suunto.SuuntoMove

object XMLParser {private val log: Logger = Logger.getLogger(classOf[XMLParser])}

class XMLParser @throws[Exception]
(var xmlFile: File) {
  XMLParser.log.debug("Parsing " + xmlFile.getName)
  val dbFactory: DocumentBuilderFactory = DocumentBuilderFactory.newInstance
  val dBuilder: DocumentBuilder = dbFactory.newDocumentBuilder
  val document: Document = dBuilder.parse(xmlFile)
  val movesCountList: NodeList = document.getElementsByTagName("MovesCount")
  if (movesCountList.getLength != 1) {
    throw new Exception("not valid moves count xml file, MovesCount node count: " + movesCountList.getLength)
  }
  val movesCount: Element = movesCountList.item(0).asInstanceOf[Element]
  val movesList: NodeList = movesCount.getElementsByTagName("Moves")
  if (movesList.getLength != 1) {
    throw new Exception("not valid moves count xml file, Moves node count: " + movesList.getLength)
  }
  moves = movesList.item(0).asInstanceOf[Element]
  final private val durationPattern: Pattern = Pattern.compile("(\\d+):(\\d+):(\\d+)\\.?(\\d*)")
  private var moves: Element = null
  @throws[Exception]
  def parse: Array[SuuntoMove] = {
    val moveList: NodeList = moves.getElementsByTagName("Move")
    if (moveList.getLength == 0) {
      XMLParser.log.debug("No moves data in " + xmlFile.getName)
      return null
    }
    val suuntoMoves: util.ArrayList[SuuntoMove] = new util.ArrayList[SuuntoMove]
    XMLParser.log.debug(moveList.getLength + " move elements in this file")
    var i: Int = 0
    while (i < moveList.getLength) {
      {
        try {
          val move: Element = moveList.item(i).asInstanceOf[Element]
          val suuntoMove: SuuntoMove = new SuuntoMove
          val header: Element = move.getElementsByTagName("Header").item(0).asInstanceOf[Element]
          parseHeader(header, suuntoMove)
          val samples: Element = move.getElementsByTagName("Samples").item(0).asInstanceOf[Element]
          parseSamples(samples, suuntoMove)
          suuntoMoves.add(suuntoMove)
        }
        catch {
          case e: Exception => {
            XMLParser.log.info("Data invalid in the no. " + (i + 1) + " of the moves")
          }
        }
      }
      ({i += 1; i})
    }
    if (suuntoMoves.size == 0) {
      return null
    }
    val moves: Array[SuuntoMove] = new Array[SuuntoMove](suuntoMoves.size)
    suuntoMoves.toArray(moves)
    return moves
  }
  private def parseSamples(samples: Element, suuntoMove: SuuntoMove) {
    val distanceStr: String = getChildElementValue(samples, "Distance")
    val heartRateStr: String = getChildElementValue(samples, "HR")
    var currentSum: Int = 0
    for (distance <- distanceStr.split(" ")) {
      if (distance.trim.isEmpty) {
        continue //todo: continue is not supported
      }
      currentSum += distance.toInt
      suuntoMove.addDistanceSample(currentSum)
    }
    for (heartRate <- heartRateStr.split(" ")) {
      if (heartRate.trim.isEmpty) {
        continue //todo: continue is not supported
      }
      suuntoMove.addHeartRateSample(heartRate.toInt)
    }
  }
  @throws[ParseException]
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
    val child: Element = parent.getElementsByTagName(elementName).item(0).asInstanceOf[Element]
    return child.getTextContent
  }
}