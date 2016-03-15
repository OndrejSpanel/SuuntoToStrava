package net.suunto3rdparty

import moveslink.MovesLinkUploader
import moveslink2.MovesLink2Uploader
import org.apache.log4j.Logger

object Main extends App {
  val log = Logger.getLogger(classOf[App])

  val mlf = MovesLinkUploader
  val ml2f = MovesLink2Uploader

  log.info("Reading MovesLink2 ...")
  if (!ml2f.checkIfEnvOkay || !mlf.checkIfEnvOkay) {
    throw new UnsupportedOperationException()
  }
  val index = ml2f.readXMLFiles()
  log.info("Reading MovesLink2 done.")
  log.info("Reading MovesLink ...")
  mlf.uploadXMLFiles(index)
  log.info("Upload MovesLink done.")

}

