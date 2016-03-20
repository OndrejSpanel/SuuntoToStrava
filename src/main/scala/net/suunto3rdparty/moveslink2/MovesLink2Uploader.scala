package net.suunto3rdparty
package moveslink2

import java.io.File

import org.apache.log4j.Logger

object MovesLink2Uploader {
  private val log = Logger.getLogger(getClass)

  private def getDataFolder: File = {
    val folderName = "Moveslink2"
    val suuntoHome = Util.getSuuntoHome
    if (suuntoHome == null) {
      return null
    }
    new File(suuntoHome, folderName)
  }

  def checkIfEnvOkay: Boolean = {
    val folder: File = getDataFolder
    if (!folder.exists) {
      MovesLink2Uploader.log.info("Cannot find MovesLink2 data folder at " + folder.getAbsolutePath)
      return false
    }
    true
  }

  def readXMLFiles(alreadyUploaded: Set[String]): Set[Move] = {
    var index = Set[Move]()
    val folder = getDataFolder
    val files = folder.listFiles
    for {
      file <- files
      fileName = file.getName.toLowerCase
      if !alreadyUploaded.contains(fileName) && fileName.endsWith(".sml")
    } yield {
      MovesLink2Uploader.log.info("Analyzing " + fileName)
      val parsed = XMLParser.parse(fileName, file)
      parsed.foreach { move =>
        index += move
        println(s"GPS: ${move.toLog}")
      }
      parsed
    }
    index
  }
}