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

  def readXMLFiles(): MoveIndex = {
    val index = new MoveIndex
    val folder = getDataFolder
    val files = folder.listFiles
    for (file <- files) {
      val fileName = file.getName.toLowerCase
      if ((fileName.startsWith("log-") && fileName.endsWith(".xml")) || fileName.endsWith(".sml")) {
        MovesLink2Uploader.log.info("Analyzing " + fileName)
        val parser = XMLParser.parse(file)
        parser.foreach { move =>
          index.add(move)
          println(s"GPS: ${move.toLog}")
        }
      }
    }
    index
  }
}