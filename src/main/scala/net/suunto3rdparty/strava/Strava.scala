package net.suunto3rdparty
package strava

import java.io._
import java.util.zip.GZIPOutputStream

import fit.Export
import org.apache.http.client.HttpResponseException
import org.apache.http.client.fluent.{Form, Request}
import org.apache.http.entity.ContentType
import org.apache.http.entity.mime.MultipartEntityBuilder

import scala.util.parsing.json.JSON

case class StravaAPIParams(appId: Int, clientSecret: String, code: Option[String])

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

class StravaAPI(appId: Int, clientSecret: String, code: Option[String]) {

  import StravaAPI._

  protected def this(params: StravaAPIParams) = this(params.appId, params.clientSecret, params.code)
  // see https://strava.github.io/api/

  lazy val authString: Option[String] = if (localTest) Some("Bearer xxxxx")
  else {
    code.map { code =>
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
    }.getOrElse(None)
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

  def uploadRawFile(moveBytesOriginal: Array[Byte]): Option[Int] = {
    import resource._

    val baos = new ByteArrayOutputStream()
    managed(new GZIPOutputStream(baos)).foreach(_.write(moveBytesOriginal))

    val sendBytes = baos.toByteArray

    try {
      authString.flatMap { authString =>
        // see https://strava.github.io/api/v3/uploads/ -
        val body = MultipartEntityBuilder.create()
          .addTextBody("data_type", "fit.gz") // case insensitive - possible values: fit, fit.gz, tcx, tcx.gz, gpx, gpx.gz
          .addTextBody("private", "1")
          .addBinaryBody("file", sendBytes, ContentType.APPLICATION_OCTET_STREAM, "file.fit.gz")
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
                println(s"  upload id ${id.toInt}")
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

    val appId = 8138
    val callbackPort = 8080
    val code = StravaAuth(appId,callbackPort, "view_private,write")

    // now wait until the auth is done

    val clientSecret = "70c838c9c54b46aaaa730d7071c3364e480d832c"
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