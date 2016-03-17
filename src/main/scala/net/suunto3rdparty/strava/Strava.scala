package net.suunto3rdparty
package strava

import java.io._

import fit.Export
import org.apache.http.client.HttpResponseException
import org.apache.http.client.fluent.{Form, Request}
import org.apache.http.entity.ContentType
import org.apache.http.entity.mime.MultipartEntityBuilder

import scala.util.parsing.json.JSON

case class StravaAPIParams(appId: Int, clientSecret: String, code: String)

object StravaAPI {
  val localTest = false

  val stravaSite = "www.strava.com"
  val stravaRootURL = "/api/v3/"
  val stravaRoot = "https://" + stravaSite + stravaRootURL

  def buildURI(path: String): String = {
    if (!localTest) stravaRoot + path
    else "http://localhost/JavaEE-war/webresources/generic/"
  }

  class CC[T] {
    def unapply(a: Option[Any]): Option[T] = a.map(_.asInstanceOf[T])
  }

  object M extends CC[Map[String, Any]]

  object L extends CC[List[Any]]

  object S extends CC[String]

  object D extends CC[Double]

  object B extends CC[Boolean]

  def buildPostData(params: (String, String)*) = {
    params.map(p => s"${p._1}=${p._2}").mkString("&")
  }
}

class StravaAPI(appId: Int, clientSecret: String, code: String) {

  import StravaAPI._

  protected def this(params: StravaAPIParams) = this(params.appId, params.clientSecret, params.code)
  // see https://strava.github.io/api/

  lazy val authString: Option[String] = if (localTest) Some("Bearer xxxxx")
  else {
    val request = Request.Post(buildURI("oauth/token"))
      .bodyForm(
        Form.form()
          .add("client_id", appId.toString)
          .add("client_secret", clientSecret)
          .add("code", code).build()
      )

    val result = request.execute().returnContent()

    val resultJson = JSON.parseFull(result.asString())

    Option(resultJson).flatMap {
      case M(map) =>
        map.get("access_token") match {
          case S(accessToken) =>
            Some("Bearer " + accessToken)
          case _ =>
            None
        }
      case _ => None
    }
  }

  def athlete: Option[String] = {
    val response = authString.map { authString =>
      val request = Request.Get(buildURI("athlete")).addHeader("Authorization", authString)

      val result = request.execute().returnContent()
      result.asString()

    }
    response
  }

  /*
    * @return upload id (use to check status with uploads/:id)
    * */
  def upload(move: Move): Option[Int] = {
    val moveBytes = Export.toBuffer(move)
    uploadRawFile(moveBytes)
  }

  def uploadRawFile(moveBytes: Array[Byte]): Option[Int] = {
    try {
      authString.flatMap { authString =>

        val body = MultipartEntityBuilder.create()
          .addTextBody("activity_type", "run")
          .addTextBody("data_type", "fit")
          .addTextBody("private", "1")
          .addBinaryBody("file", moveBytes, ContentType.APPLICATION_OCTET_STREAM, "file.fit")
          .build()

        val request = Request.Post(buildURI("uploads"))
          .useExpectContinue()
          .addHeader("Authorization", authString)
          .addHeader("Accept", "*/*")
          .body(body)

        val response = request.execute()
        val content = response.returnContent()

        val resultString = content.asString()

        // we expect to receive 201
        // TODO: check if result is OK

        val resultJson = JSON.parseFull(resultString)
        val uploadId = Option(resultJson).flatMap {
          case M(map) =>
            map.get("id") match {
              case D(id) =>
                Some(id.toInt)
              case _ =>
                None
            }
          case _ => None
        }

        uploadId

      }
    } catch {
      case ex: HttpResponseException =>
        // we expect to receive error 400 - duplicate activity
        println(ex.getMessage)
        None
      case ex: Exception =>
        ex.printStackTrace()
        None
    }
  }

}

object StravaAPIThisApp {

  def getAPIParams: StravaAPIParams = {
    val home = new File(Util.getSuuntoHome, "Moveslink")
    val appId = 8138
    val tokenFile = new File(home, "strava.id")

    val source = scala.io.Source.fromFile(tokenFile)
    val (clientSecret, token, code) = try {
      val lines = source.getLines()
      val secret = lines.next
      val token = lines.next
      val code = lines.next
      (secret, token, code)
    } finally source.close()
    StravaAPIParams(appId, clientSecret, code)

  }
}

class StravaAPIThisApp extends StravaAPI(StravaAPIThisApp.getAPIParams) {
}

object StravaAccess extends App {
  val api = new StravaAPIThisApp

  for (athlete <- api.athlete) {
    println(athlete)
  }

  // try uploading a fit file
  val toUpload = getClass.getResourceAsStream("/uploadTest.fit")

  if (toUpload != null) {
    val buffer = Array.ofDim[Byte](4096)
    val ous = new ByteArrayOutputStream()
    var read = 0
    do {
      read = toUpload.read(buffer)
      if (read > 0) ous.write(buffer, 0, read)
    } while (read > 0)
    ous.close()
    toUpload.close()

    api.uploadRawFile(ous.toByteArray)
  }
}