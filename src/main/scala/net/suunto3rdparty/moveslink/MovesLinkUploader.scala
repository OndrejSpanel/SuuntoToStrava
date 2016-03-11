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

  def uploadXMLFiles(): Unit = {
    val folder = getDataFolder
    /*
    val noMovesFolder: File = new File(folder, "NoMoves")
    val duplicateMovesFolder: File = new File(folder, "Duplicates")
    val pendingMovesFolder: File = new File(folder, "Pending")
    val uploadedMovesFolder: File = new File(folder, "Uploaded")
    noMovesFolder.mkdir
    duplicateMovesFolder.mkdir
    pendingMovesFolder.mkdir
    uploadedMovesFolder.mkdir
    */
    val files = folder.listFiles
    for (file <- files) {
      val fileName = file.getName.toLowerCase
      if (fileName.startsWith("quest_") && fileName.endsWith(".xml")) {
        log.info("Analyzing " + fileName)
        val moves = XMLParser.parse(file)
        if (moves.isEmpty) {
          log.info("There's no moves in " + file.getName)
          //file.renameTo(new File(noMovesFolder, file.getName))
        } else {
          log.info("Moving file into pending folder: " + file.getName)
          //file.renameTo(new File(pendingMovesFolder, file.getName))
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