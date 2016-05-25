package net.suunto3rdparty
package moveslink2

import java.io.File
import java.time.ZonedDateTime
import Util._

import org.apache.log4j.Logger

object MovesLink2Uploader {
  private val log = Logger.getLogger(getClass)

  private val getDataFolder: File = {
    val folderName = "Moveslink2"
    val suuntoHome = getSuuntoHome
    if (suuntoHome == null) {
      throw new UnsupportedOperationException("Suunto home not found")
    }
    new File(suuntoHome, folderName)
  }

  def checkIfEnvOkay: Boolean = {
    if (!getDataFolder.exists) {
      MovesLink2Uploader.log.info("Cannot find MovesLink2 data folder at " + getDataFolder.getAbsolutePath)
      return false
    }
    true
  }

  def listFiles: Set[String] = getDataFolder.list.toSet.filter(_.endsWith(".sml"))

  def readXMLFiles(after: Option[ZonedDateTime], alreadyUploaded: Set[String], progress: (Int, Int) => Unit): Set[Move] = {
    val toRead = listFiles diff alreadyUploaded
    val total = toRead.size
    progress(0, total)
    var processed = 0
    val indexed = for (fileName <- toRead) yield {
      MovesLink2Uploader.log.info("Analyzing " + fileName)
      val parsed = XMLParser.parse(fileName, new File(getDataFolder, fileName))
      val parsedAfter = parsed.filter { move =>
        after.isEmpty || move.startTime.exists(_ >= after.get)
      }
      processed += 1
      progress(processed, toRead.size)
      parsedAfter.foreach(move => println(s"GPS: ${move.toLog}"))
      parsedAfter.toOption
    }
    indexed.flatten
  }
}