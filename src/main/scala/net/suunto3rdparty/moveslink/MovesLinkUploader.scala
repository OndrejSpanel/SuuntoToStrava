package net.suunto3rdparty
package moveslink

import java.io.File

import strava.StravaAPIThisApp
import Util._
import org.apache.log4j.Logger

import scala.annotation.tailrec

object MovesLinkUploader {
  private val log = Logger.getLogger(getClass)

  private def getDataFolder: File = {
    val suuntoHome = Util.getSuuntoHome
    new File(suuntoHome, "Moveslink")
  }

  def uploadXMLFiles(index: Set[Move]): Unit = {
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

    val timelineGPS = index.toList.filterNot(_.isAlmostEmpty(30)).sorted
    val timelineHR = questMoves.toList.filterNot(_.isAlmostEmpty(30)).sorted

    @tailrec
    def processTimelines(lineGPS: List[Move], lineHRD: List[Move], processed: List[Move]): List[Move] = {
      def prependNonEmpty(move: Option[Move], list: List[Move]): List[Move] = {
        move.find(!_.isAlmostEmpty(30)).toList ++ list
      }

      if (lineGPS.isEmpty) {
        if (lineHRD.isEmpty) {
          processed
        } else {
          // HR moves without GPS info
          processTimelines(lineGPS, lineHRD.tail, prependNonEmpty(lineHRD.headOption, processed))
        }
      } else if (lineHRD.isEmpty) {
        processTimelines(lineGPS.tail, lineHRD, prependNonEmpty(lineGPS.headOption, processed))
      } else {
        val hrdMove = lineHRD.head
        val gpsMove = lineGPS.head

        val gpsBeg = gpsMove.startTime.get
        val gpsEnd = gpsMove.endTime.get

        val hrdBeg = hrdMove.startTime.get
        val hrdEnd = hrdMove.endTime.get

        if (gpsBeg >= hrdEnd) {
          // no match for hrd
          processTimelines(lineGPS, lineHRD.tail, prependNonEmpty(lineHRD.headOption, processed))
        } else if (hrdBeg > gpsEnd) {
          processTimelines(lineGPS.tail, lineHRD, prependNonEmpty(lineGPS.headOption, processed))
        } else {
          // some overlap, handle it
          if (gpsBeg > hrdBeg) {
            val (takeHRD, leftHRD) = hrdMove.takeUntil(gpsBeg)

            processTimelines(lineGPS, prependNonEmpty(leftHRD, lineHRD.tail), prependNonEmpty(takeHRD, processed))

          } else if (gpsBeg < hrdBeg) {
            val (takeGPS, leftGPS) = gpsMove.takeUntil(hrdBeg)

            processTimelines(prependNonEmpty(leftGPS, lineGPS.tail), lineHRD, prependNonEmpty(takeGPS, processed))
          } else {
            // same beginning - drive by HRD
            // use from GPS only as needed by HRD
            val (takeGPS, leftGPS) = gpsMove.takeUntil(hrdEnd)
            val merged = takeGPS.map(_.streams(StreamGPS)).map(hrdMove.addStream)
            println(s"Merged GPS ${takeGPS.map(_.toLog)} into ${hrdMove.toLog}")

            processTimelines(prependNonEmpty(leftGPS, lineGPS.tail), prependNonEmpty(merged, lineHRD.tail), processed)
          }

        }

      }
    }

    val toUpload = processTimelines(timelineGPS, timelineHR, Nil).reverse

    toUpload.foreach { move =>
      println(s"Uploading: ${move.toLog}")
      // upload each move separately
      fit.Export(move)
      // TODO: when activities are close enough and share a common HR move, merge them
      // upload only non-trivial results
      if (!move.isAlmostEmpty(120)) {
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