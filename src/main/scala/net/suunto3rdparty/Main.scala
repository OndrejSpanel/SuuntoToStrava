package net.suunto3rdparty

import moveslink.MovesLinkUploader
import moveslink2.MovesLink2Uploader
import strava.{StravaAPIThisApp, StravaAuth}
import org.apache.log4j.Logger

object Main extends App {
  val log = Logger.getLogger(classOf[App])

  val api = new StravaAPIThisApp

  if (api.authString.nonEmpty) {

    val mlf = MovesLinkUploader
    val ml2f = MovesLink2Uploader

    log.info("Reading MovesLink2 ...")
    if (!ml2f.checkIfEnvOkay || !mlf.checkIfEnvOkay) {
      throw new UnsupportedOperationException()
    }
    val alreadyUploaded = mlf.listAlreadyUploaded()

    val index = ml2f.readXMLFiles(alreadyUploaded)
    log.info("Reading MovesLink2 done.")
    log.info("Reading MovesLink ...")
    val uploaded = mlf.uploadXMLFiles(api, alreadyUploaded, index, (num, total) => StravaAuth.progress(s"Processing $num of $total files"))
    log.info("Upload MovesLink done.")
    StravaAuth.stop(s"Completed, moves uploaded: $uploaded ")
  } else {
    StravaAuth.stop("Canceled")
  }
}
