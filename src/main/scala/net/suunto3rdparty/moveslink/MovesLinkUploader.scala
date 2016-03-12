package net.suunto3rdparty
package moveslink

import java.io.File

import org.apache.log4j.Logger

object MovesLinkUploader {
  private val log = Logger.getLogger(getClass)

  private def getDataFolder: File = {
    val suuntoHome = Util.getSuuntoHome
    new File(suuntoHome, "Moveslink")
  }

  def uploadXMLFiles(index: MoveIndex): Unit = {
    val folder = getDataFolder
    val files = folder.listFiles
    for (file <- files) {
      val fileName = file.getName.toLowerCase
      if (fileName.startsWith("quest_") && fileName.endsWith(".xml")) {
        log.info("Analyzing " + fileName)
        val moves = XMLParser.parse(file)
        moves.foreach{ move =>
          println(s"Quest HR: ${move.startTime}..${move.endTime}")
          // upload each move separately
          val gpsData = index.listOverlapping(move)
          if (gpsData.nonEmpty) {
            val merged = gpsData.reduce(_ mergeGPS _)
            println(s"  GPS found: ${merged.startTime}..${merged.endTime}")
          }
          // TODO: handle GPS data with no HR - upload them separately
        }
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