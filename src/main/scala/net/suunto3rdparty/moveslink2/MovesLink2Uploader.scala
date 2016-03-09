package net.suunto3rdparty
package moveslink2

import java.io.File

import org.apache.log4j.Logger


object MovesLink2Uploader {private val log = Logger.getLogger(classOf[MovesLink2Uploader])}

class MovesLink2Uploader {
  private def getDataFolder: File = {
    var folderName = "Moveslink2"
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
    if (!folder.canWrite) {
      MovesLink2Uploader.log.error("Cannot write to moveslink2 data folder at " + folder.getAbsolutePath)
      return false
    }
    true
  }

  def uploadXMLFiles(): Unit = {
    val folder = getDataFolder
    //val notRunFolder = new File(folder, "NotRun")
    //val uploadedMovesFolder = new File(folder, "Uploaded")
    //notRunFolder.mkdir
    //uploadedMovesFolder.mkdir
    val files = folder.listFiles
    for (file <- files) {
      val fileName = file.getName.toLowerCase
      if ((fileName.startsWith("log-") && fileName.endsWith(".xml")) || fileName.endsWith(".sml")) {
        MovesLink2Uploader.log.info("Analyzing " + fileName)
        val parser = new XMLParser(file)
        if (parser.isParseCompleted) {
          //uploadMoveToNike(nikePlus, parser.getSuuntoMove)
          //file.renameTo(new File(uploadedMovesFolder, file.getName))
        }
        else {
          //file.renameTo(new File(notRunFolder, file.getName))
        }
      }
    }
  }
}