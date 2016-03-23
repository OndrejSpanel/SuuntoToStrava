package net.suunto3rdparty
package moveslink2

import java.io.File

import org.apache.log4j.Logger

object MovesLink2Uploader {
  private val log = Logger.getLogger(getClass)

  private val getDataFolder: File = {
    val folderName = "Moveslink2"
    val suuntoHome = Util.getSuuntoHome
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

  def readXMLFiles(alreadyUploaded: Set[String]): Set[Move] = {
    val indexed = for {
      fileName <- listFiles
      if !alreadyUploaded.contains(fileName)
    } yield {
      MovesLink2Uploader.log.info("Analyzing " + fileName)
      val parsed = XMLParser.parse(fileName, new File(getDataFolder, fileName))
      parsed.foreach { move =>
        println(s"GPS: ${move.toLog}")
      }
      parsed.toOption
    }
    indexed.flatten
  }
}