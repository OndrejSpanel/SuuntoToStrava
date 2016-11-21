package net.suunto3rdparty
package moveslink

import java.io.{File, FileInputStream, IOException}
import org.joda.time.{DateTime=>ZonedDateTime}
import java.util.Properties

import strava.StravaAPI
import Util._
import org.apache.log4j.Logger
import resource._

import scala.annotation.tailrec

object MovesLinkUploader {
  val fileTest = false

  private val log = Logger.getLogger(getClass)

  val getDataFolder: File = {
    val suuntoHome = Util.getSuuntoHome
    new File(suuntoHome, "Moveslink")
  }

  private val uploadedFolderName = "/uploadedToStrava"

  private val settings = Settings

  private val uploadedFolder = new File(getDataFolder, uploadedFolderName)

  // Strava id + list of source filenames
  case class UploadId(id: Long, filenames: Seq[String])

  def uploadXMLFiles(after: Option[ZonedDateTime], api: StravaAPI, alreadyUploaded: Set[String], index: Set[Move], progress: (Int, Int) => Unit): List[UploadId] = {


    try {
      uploadedFolder.mkdir()
    } catch {
      case _ : SecurityException => // expected (can already exist)
    }

    val questMovesGather = for {
      fileName <- listFiles
      if !alreadyUploaded.contains(fileName)
    } yield {
      log.info("Analyzing " + fileName)
      val file = new File(getDataFolder, fileName)
      val moves = XMLParser.parse(fileName, file)
      val validMoves = moves.filter(_.streamGet[DataStreamHRWithDist].nonEmpty)

      val validMovesAfter = validMoves.filter(_.startsAfter(after))

      val skipped = validMoves diff validMovesAfter

      skipped.foreach(markUploaded)

      validMovesAfter.foreach(move => println(s"Quest HR: ${move.toLog}"))
      if (validMovesAfter.isEmpty) {
        markUploadedFile(fileName)
      }

      validMovesAfter
    }

    val questMoves = questMovesGather.toSeq.flatten

    // create overlapping timelines Quest / GPS
    val ignoreDuration = 30

    (index ++ questMoves).filter(_.isAlmostEmpty(ignoreDuration)).foreach(markUploaded)

    val timelineGPS = index.toList.filterNot(_.isAlmostEmpty(ignoreDuration)).sorted
    val timelineHR = questMoves.toList.filterNot(_.isAlmostEmpty(ignoreDuration)).sorted

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
          // check if the activity start is the same within a tolerance

          // 10 percent means approx. 5 minutes from 1 hour (60 minutes)
          val tolerance = (lineGPS.head.duration max lineHRD.head.duration) * 0.10f

          if (timeDifference(gpsBeg, hrdBeg).abs <= tolerance) {
            // same beginning - drive by HRD
            // use from GPS only as needed by HRD
            // if GPS is only a bit longer than HDR, use it whole, unless there is another HDR waiting for it
            val (takeGPS, leftGPS) = if (timeDifference(gpsEnd, hrdEnd).abs <= tolerance && lineHRD.tail.isEmpty) {
              (Some(gpsMove), None)
            } else {
              gpsMove.span(hrdEnd)
            }

            val merged = takeGPS.map(m => (m.stream[DataStreamGPS], m)).map { sm =>
              val hrdAdjusted = sm._1.adjustHrd(hrdMove)
              hrdAdjusted.addStream(sm._2, sm._1)
            }

            println(s"Merged GPS ${takeGPS.map(_.toLog)} into ${hrdMove.toLog}")

            processTimelines(prependNonEmpty(leftGPS, lineGPS.tail), prependNonEmpty(merged, lineHRD.tail), processed)
          } else if (gpsBeg > hrdBeg) {
            val (takeHRD, leftHRD) = hrdMove.span(gpsBeg)

            processTimelines(lineGPS, prependNonEmpty(leftHRD, lineHRD.tail), prependNonEmpty(takeHRD, processed))

          } else  {
            val (takeGPS, leftGPS) = gpsMove.span(hrdBeg)

            processTimelines(prependNonEmpty(leftGPS, lineGPS.tail), lineHRD, prependNonEmpty(takeGPS, processed))
          }
        }

      }
    }


    val experimentalTimeAdjust = -23
    val timelineHRAdjusted = timelineHR.map(_.timeOffset(settings.questTimeOffset + experimentalTimeAdjust))

    val toUpload = processTimelines(timelineGPS, timelineHRAdjusted, Nil).reverse

    var uploaded = List[UploadId]()
    var processed = 0
    val total = toUpload.size

    toUpload.foreach { move =>
      println(s"Uploading: ${move.toLog}")

      if (fileTest) {
        fit.Export(move)
        uploaded = UploadId(0L, move.fileName.toSeq) +: uploaded
      } else {
        // upload only non-trivial results
        if (!move.isAlmostEmpty(90)) {
          val uploadId = api.upload(move)
          uploaded = uploaded ++ uploadId.map(id => UploadId(id, move.fileName.toSeq))
        }

        markUploaded(move)
      }
      processed += 1

      progress(processed, total)
    }
    uploaded
  }

  def markUploaded(move: Move): Unit = {
    for (filename <- move.fileName) {
      markUploadedFile(filename)
    }
  }
  def markUploadedFile(filename: String): Unit = {
    try {
      if (!fileTest) {
        val markFile = new File(uploadedFolder, "/" + filename)
        markFile.createNewFile()
      }
    } catch {
      case _: IOException =>
    }
  }
  def unmarkUploadedFile(filename: String): Unit = {
    try {
      if (!fileTest) {
        val markFile = new File(uploadedFolder, "/" + filename)
        markFile.delete()
      }
    } catch {
      case _: IOException =>
    }
  }
  def listFiles: Set[String] = getDataFolder.list.toSet.filter(name => name.endsWith(".xml") && name.toLowerCase.startsWith("quest_"))

  def listAlreadyUploaded(): Set[String] = {
    val uploaded = new File(getDataFolder, uploadedFolderName)
    val files = uploaded.listFiles
    if (files == null) Set()
    else {
      files.map(_.getName)(collection.breakOut)
    }
  }

  def pruneObsolete(strings: Set[String]): Unit = {
    for (file <- strings) {
      // I use .x during debugging as temporary notes
      if (!file.endsWith(".x")) {
        val toRemove = new File(getDataFolder, file)
        toRemove.delete()
      }
    }
  }


  def checkIfEnvOkay: Boolean = {
    if (!getDataFolder.exists) {
      log.info("Cannot find MovesLink data folder at " + getDataFolder.getAbsolutePath)
      return false
    }
    if (!getDataFolder.canWrite) {
      log.error("Cannot write to moves link data folder at " + getDataFolder.getAbsolutePath)
      return false
    }
    true
  }
}