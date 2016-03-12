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
          println(s"Quest HR: ${move.toLog}")
          // upload each move separately
          val gpsData = index.listOverlapping(move.streams(StreamHR), StreamGPS)
          val merged = gpsData.foldLeft(move)(_ addStream _)
          // if no GPS data found, upload the move without them
          println(s"  GPS merged: ${gpsData.map(_.toLog).mkString(", ")}")
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