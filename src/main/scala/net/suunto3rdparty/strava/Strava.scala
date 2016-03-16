package net.suunto3rdparty
package strava

import java.io.{ByteArrayInputStream, File}

import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.scaladsl.{Sink, Source}
import HttpMethods._
import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.MediaTypes.multipart
import akka.http.scaladsl.model.headers.{Accept, Expect, RawHeader}
import akka.stream.ActorMaterializer
import fit.Export

import scala.concurrent.duration.Duration
import scala.util.parsing.json.JSON
import scala.concurrent.Await

case class StravaAPIParams(appId: Int, clientSecret: String, code: String)

object StravaAPI {
  val localTest = true

  val stravaSite = "www.strava.com"
  val stravaRootURL = "/api/v3/"
  val stravaRoot = "https://" + stravaSite + stravaRootURL

  def buildURI(path: String): Uri = {
    if (!localTest) Uri(scheme = "https", authority = Uri.Authority(Uri.Host(stravaSite)), path = Uri.Path(stravaRootURL + path))
    else Uri(scheme = "http", authority = Uri.Authority(Uri.Host("localhost"), port = 8080), path = Uri.Path("/JavaEE-war/webresources/generic/"))
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

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  import scala.concurrent.ExecutionContext.Implicits.global

  lazy val authString: Option[String] = if (localTest) Some("Bearer xxxxx") else {
    val target = buildURI("oauth/token")
    val formData = FormData(
      ("client_id", appId.toString),
      ("client_secret", clientSecret),
      ("code", code)
    )
    val request = HttpRequest(POST, target, entity = formData.toEntity)

    val result = for {
      response <- Http().singleRequest(request)
      responseBodyAsString <- Unmarshal(response).to[String]
    } yield {
      val tokenString = responseBodyAsString

      response.entity.dataBytes.runWith(Sink.ignore)

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

    Await.result(result, Duration.Inf)
  }

  /*

  def createEntity(file: File): Future[RequestEntity] = {
    require(file.exists())
    val formData =
      Multipart.FormData(
        Source.single(
          Multipart.FormData.BodyPart(
            "test",
            HttpEntity(MediaTypes.`application/octet-stream`, file.length(), SynchronousFileSource(file, chunkSize = 100000)), // the chunk size here is currently critical for performance
            Map("filename" -> file.getName))))
    Marshal(formData).to[RequestEntity]
  }
  */


  def athlete: Option[String] = {
    val response = authString.map { authString =>
      val result = {
        val target = buildURI("athlete")
        val http = HttpRequest(GET, uri = target, headers = List(RawHeader("Authorization", authString)))

        for {
          response <- Http().singleRequest(http)
          responseBodyAsString <- Unmarshal(response).to[String]
        } yield {
          response.entity.dataBytes.runWith(Sink.ignore)
          responseBodyAsString
        }
      }

      Await.result(result, Duration.Inf)

    }

    response
  }

  /*
    * @return upload id (use to check status with uploads/:id)
    * */
  def upload(move: Move): Option[Int] = {
    val response = authString.flatMap{ authString =>
      val moveBytes = Export.toBuffer(move)
      val is = new ByteArrayInputStream(moveBytes)

      val target = buildURI("uploads")

      val pars = List(
        ("activity_type", "run"), //
        ("data_type", "fit"),
        ("private", "1")
      )
      val parsBodyPart = pars.map { kv =>
        val contentType = ContentType(MediaTypes.`multipart/form-data`, () => HttpCharsets.`US-ASCII`)
        Multipart.FormData.BodyPart.Strict(kv._1, HttpEntity(contentType, kv._2.getBytes))
      }
      val filename = Map("filename" ->"file.fit")
      val fileBodyPart = Multipart.FormData.BodyPart.Strict("file", HttpEntity(ContentTypes.`application/octet-stream`, moveBytes), additionalDispositionParams = filename)

      val multipartForm = Multipart.FormData.Strict(parsBodyPart :+ fileBodyPart)

      val multipartEntity = Marshal(multipartForm).to[RequestEntity]

      val uploadRequest = for {
        entity <- multipartEntity
        //headers = List(RawHeader("Authorization", authString), Expect.`100-continue`, Accept(PredefinedMediaRange("*/*")))
        headers = List(RawHeader("Authorization", authString), Expect.`100-continue`, Accept(MediaRanges.`*/*`))
        request = HttpRequest(POST, target, headers = headers, entity = entity)
        response <- Http().singleRequest(request)
        responseBodyAsString <- Unmarshal(response).to[String]
      } yield {
        println(entity)
        println(request)
        responseBodyAsString
      }
      val result = Await.result(uploadRequest, Duration.Inf)
      // TODO: check if result is OK
      // we expect to receive 201
      val resultJson = JSON.parseFull(result)
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