package net.suunto3rdparty

import moveslink.MovesLinkUploader
import moveslink2.MovesLink2Uploader
import strava.{StravaAPIThisApp, StravaAuth}
import org.apache.log4j.Logger

object Main extends App {
  val log = Logger.getLogger(classOf[App])

  val api = new StravaAPIThisApp

  if (api.authString.nonEmpty) {

    val after = api.mostRecentActivityTime

    log.info("Reading MovesLink2 ...")
    if (!MovesLink2Uploader.checkIfEnvOkay || !MovesLinkUploader.checkIfEnvOkay) {
      StravaAuth.stop("Moveslink not installed correctly")
      throw new UnsupportedOperationException()
    }
    try {
      val alreadyUploaded = MovesLinkUploader.listAlreadyUploaded()
      val filesToProcess = MovesLinkUploader.listFiles ++ MovesLink2Uploader.listFiles

      MovesLinkUploader.pruneObsolete(alreadyUploaded -- filesToProcess)

      val index = MovesLink2Uploader.readXMLFiles(after, alreadyUploaded)
      log.info("Reading MovesLink2 done.")
      log.info("Reading MovesLink ...")
      val uploaded = MovesLinkUploader.uploadXMLFiles(after, api, alreadyUploaded, index, (num, total) => StravaAuth.progress(s"Processing $num of $total files"))
      log.info("Upload MovesLink done.")
      StravaAuth.stop(s"Completed, moves uploaded: $uploaded ")
    } catch {
      case x: Exception =>
        StravaAuth.stop(s"Completed with exception ${x.getMessage}")
        throw x
    }
  } else {
    StravaAuth.stop("Canceled")
  }
}
