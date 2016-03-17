package net.suunto3rdparty
package moveslink

import java.io.File
import java.time.ZonedDateTime

import strava.StravaAPIThisApp
import Util._
import org.apache.log4j.Logger

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

object MovesLinkUploader {
  private val log = Logger.getLogger(getClass)

  private def getDataFolder: File = {
    val suuntoHome = Util.getSuuntoHome
    new File(suuntoHome, "Moveslink")
  }

  def uploadXMLFiles(index: MoveIndex): Unit = {
    val api = new StravaAPIThisApp

    val folder = getDataFolder
    val files = folder.listFiles
    val questMovesGather = for {
      file <- files
      fileName = file.getName.toLowerCase
      if fileName.startsWith("quest_") && fileName.endsWith(".xml")
    } yield {
      log.info("Analyzing " + fileName)
      val moves = XMLParser.parse(file)
      val validMoves = moves.filter(_.streams.contains(StreamHRWithDist))

      validMoves.foreach(move => println(s"Quest HR: ${move.toLog}"))

      validMoves
    }

    val questMoves = questMovesGather.toSeq.flatten

    // create overlapping timelines Quest / GPS
    type Timeline = SortedMap[ZonedDateTime, Move]
    val emptyTimeline = SortedMap[ZonedDateTime, Move]()

    var timelineGPS: Timeline = index.index.map(m => m.startTime -> m)(collection.breakOut)

    var timelineHR: Timeline = questMoves.map(m => m.startTime -> m)(collection.breakOut)

    @tailrec
    def processTimelines(lineGPS: List[(ZonedDateTime, Move)], lineHRD: List[(ZonedDateTime, Move)], processed: List[Move]): List[Move] = {
      if (lineGPS.isEmpty) {
        if (lineHRD.isEmpty) {
          processed
        } else {
          // HR moves without GPS info
          processTimelines(lineGPS, lineHRD.tail, lineHRD.head._2 +: processed)
        }
      } else if (lineHRD.isEmpty) {
        processTimelines(lineGPS.tail, lineHRD, lineGPS.head._2 +: processed)
      } else {
        val gpsBeg = lineGPS.head._1
        val gpsEnd = lineGPS.head._2.endTime

        val hrdBeg = lineHRD.head._1
        val hrdEnd = lineHRD.head._2.endTime

        // TODO: handle small times
        if (gpsBeg >= hrdEnd) {
          // no match for hrd
          processTimelines(lineGPS, lineHRD.tail, lineHRD.head._2 +: processed)
        } else if (hrdBeg > gpsEnd) {
          processTimelines(lineGPS.tail, lineHRD, lineGPS.head._2 +: processed)
        } else {
          // some overlap, handle it
          val hrdMove = lineHRD.head._2
          val gpsMove = lineGPS.head._2

          def prependNonEmpty(move: Move, list: List[(ZonedDateTime, Move)]) = {
            if (move.endTime > move.startTime.plusSeconds(10)) (move.startTime -> move) +: list
            else list
          }

          if (gpsBeg > hrdBeg) {
            val (takeHRD, leftHRD) = hrdMove.takeUntil(gpsBeg)

            processTimelines(lineGPS, prependNonEmpty(leftHRD, lineHRD.tail), takeHRD +: processed)

          } else if (gpsBeg < hrdBeg) {
            val (takeGPS, leftGPS) = gpsMove.takeUntil(hrdBeg)

            processTimelines(prependNonEmpty(leftGPS, lineGPS.tail), lineHRD.tail, takeGPS +: processed)
          } else {
            // same beginning - drive by HRD
            val gpsStream = gpsMove.streams(StreamGPS)
            val merged = hrdMove.addStream(gpsStream)
            println(s"Merged GPS ${gpsStream.toLog} into ${hrdMove.toLog}")

            processTimelines(lineGPS, prependNonEmpty(merged, lineHRD.tail), processed)
          }

        }

      }
    }

    val toUpload = processTimelines(timelineGPS.toList, timelineHR.toList, Nil)

    toUpload.foreach { move =>
      println(s"Uploading: ${move.toLog}")
      // upload each move separately
      fit.Export(move)
      // upload only non-trivial results
      if (move.header.distance > 10 && move.header.durationMs > 10000) {
        api.upload(move)
      }
    }
  }

  def checkIfEnvOkay: Boolean = {
    val folder = getDataFolder
    if (!folder.exists) {
      log.info("Cannot find MovesLink data folder at " + folder.getAbsolutePath)
      return false
    }
    if (!folder.canWrite) {
      log.error("Cannot write to moves link data folder at " + folder.getAbsolutePath)
      return false
    }
    true
  }
}