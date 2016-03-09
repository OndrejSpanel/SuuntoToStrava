package net.suunto3rdparty

import moveslink.MovesLinkUploader
import moveslink2.MovesLink2Uploader
import org.apache.log4j.Logger


object Main extends App {
  val log = Logger.getLogger(classOf[App])

  private def uploadMovesLink() {
    log.info("Uploading MovesLink ...")
    val mlf = new MovesLinkUploader
    if (!mlf.checkIfEnvOkay) {
      return
    }
    mlf.uploadXMLFiles()
    log.info("Upload MovesLink done.")
  }

  private def uploadMovesLink2() {
    log.info("Uploading MovesLink2 ...")
    val ml2f = new MovesLink2Uploader
    if (!ml2f.checkIfEnvOkay) {
      return
    }
    ml2f.uploadXMLFiles()
    log.info("Upload MovesLink2 done.")
  }

  uploadMovesLink()
  uploadMovesLink2()

}
