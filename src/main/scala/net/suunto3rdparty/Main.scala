package net.suunto3rdparty

import moveslink.MovesLinkUploader
import moveslink2.MovesLink2Uploader
import strava.{StravaAPIThisApp, StravaAuth}
import org.apache.log4j.Logger

object Main extends App {
  val log = Logger.getLogger(classOf[App])

  val api = new StravaAPIThisApp

  if (api.authString.nonEmpty) {

    log.info("Reading MovesLink2 ...")
    if (!MovesLink2Uploader.checkIfEnvOkay || !MovesLinkUploader.checkIfEnvOkay) {
      throw new UnsupportedOperationException()
    }
    val alreadyUploaded = MovesLinkUploader.listAlreadyUploaded()
    val filesToProcess = MovesLinkUploader.listFiles ++ MovesLink2Uploader.listFiles

    MovesLinkUploader.pruneObsolete(alreadyUploaded -- filesToProcess)

    val index = MovesLink2Uploader.readXMLFiles(alreadyUploaded, (num, total) => StravaAuth.progress(s"Reading $num of $total GPS files"))
    log.info("Reading MovesLink2 done.")
    log.info("Reading MovesLink ...")
    val uploaded = MovesLinkUploader.uploadXMLFiles(api, alreadyUploaded, index, (num, total) => StravaAuth.progress(s"Processing $num of $total files"))
    log.info("Upload MovesLink done.")
    StravaAuth.stop(s"Completed, moves uploaded: $uploaded ")
  } else {
    StravaAuth.stop("Canceled")
  }
}
