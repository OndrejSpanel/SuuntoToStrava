package net.suunto3rdparty

import moveslink.MovesLinkUploader
import moveslink2.MovesLink2Uploader
import org.apache.log4j.Logger
import java.io.File

import scalaj.http.Http

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

object StravaAccess extends App {
  val home = new File(Util.getSuuntoHome, "Moveslink")
  val appId = 8138
  val tokenFile = new File(home, "strava.id")

  val source = scala.io.Source.fromFile(tokenFile)
  val (clientSecret, accessToken, code) = try {
    val lines = source.getLines()
    val secret = lines.next
    val token = lines.next
    val code = lines.next
    (secret, token, code)
  } finally source.close()

  val request = Http("https://www.strava.com/api/v3/oauth/token").postData(s"client_id=$appId&client_secret=$clientSecret&code=$code")

  val tokenString = request.asString

  println(tokenString)
}