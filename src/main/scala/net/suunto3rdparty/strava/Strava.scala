package net.suunto3rdparty
package strava

import java.io.{ByteArrayInputStream, File}

import fit.Export

import scala.util.parsing.json.JSON
import scalaj.http.{Http, MultiPart}

case class StravaAPIParams(appId: Int, clientSecret: String, code: String)

object StravaAPI {
  val stravaRoot = "https://www.strava.com/api/v3/"

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

  def textPart(name: String, value: String): MultiPart = {
    MultiPart(name, name, "text/plain", value)
  }
}

class StravaAPI(appId: Int, clientSecret: String, code: String) {
  import StravaAPI._

  protected def this(params: StravaAPIParams) = this(params.appId, params.clientSecret, params.code)
  // see https://strava.github.io/api/

  lazy val authString: Option[String] = {
    val request = Http(stravaRoot + "oauth/token").postData(
      buildPostData(
        ("client_id", appId.toString),
        ("client_secret", clientSecret),
        ("code", code)
      ))

    val tokenString = request.asString.body

    val resultJson = JSON.parseFull(tokenString)

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
    val response = for (authString <- authString) yield {
      val athleteRequest = Http(stravaRoot + "athlete").header("Authorization", authString)

      val body = athleteRequest.asString.body
      if (body.nonEmpty) Some(body) else None
    }
    response.flatten
  }

  /*
  * @return upload id (use to check status with uploads/:id)
  * */
  def upload(move: Move): Option[Int] = {
    val response = authString.flatMap{ authString =>
      val moveBytes = Export.toBuffer(move)
      val is = new ByteArrayInputStream(moveBytes)

      /* from https://github.com/danshannon/javastravav3api/blob/master/Strava%20API%20v3/src/javastrava/api/v3/rest/UploadAPI.java
      @Multipart
      @POST("/uploads")
      public StravaUploadResponse upload(
        @Part("activity_type") final StravaActivityType activityType, @Part("name") final String name,
        @Part("description") final String description, @Part("private") final Boolean _private,
        @Part("trainer") final Boolean trainer, @Part("commute") Boolean commute, @Part("data_type") final String dataType,
        @Part("external_id") final String externalId, @Part("file") final TypedFile file) throws BadRequestException;
      */

      /*
      public StravaUploadResponse upload(
      @Part("activity_type") final StravaActivityType activityType, @Part("name") final String name,
      @Part("description") final String description, @Part("private") final Boolean _private,
      @Part("trainer") final Boolean trainer, @Part("commute") Boolean commute, @Part("data_type") final String dataType,
      @Part("external_id") final String externalId, @Part("file") final TypedFile file) throws BadRequestException;
      */

      val uploadRequest = Http(stravaRoot + "uploads")
        .header("Authorization", authString)
        .postMulti(
          textPart("data_type", "fit"),
          textPart("activity_type", "run"), //
          textPart("name", "Test Upload Run"), //
          textPart("description", "Just testing"), //
          textPart("trainer", "0"),
          textPart("commute", "0"),
          textPart("private", "1"),
          textPart("external_id", "veryUniqueString"),
          new MultiPart("file", "upload.fit", "application/octet-stream", is, moveBytes.length, progress => ())
        )
      val result = uploadRequest.asString
      if (result.isError) None
      else {
        // TODO: check if result is OK
        val resultJson = JSON.parseFull(result.body)
        val uploadId = Option(resultJson).flatMap {
          case M(map) =>
            map.get("id") match {
              case S(id) =>
                Some(id.toInt)
              case _ =>
                None
            }
          case _ => None
        }

        uploadId

      } // we expect to receive 201

    }
    response
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
}