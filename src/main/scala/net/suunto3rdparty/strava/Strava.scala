package net.suunto3rdparty
package strava

import java.io.File

import scala.util.parsing.json.JSON
import scalaj.http.Http

class StravaAPI(appId: Int, clientSecret: String, code: String) {
  val stravaRoot = "https://www.strava.com/api/v3/"

  lazy val authString: Option[String] = {
    val request = Http(stravaRoot + "oauth/token").postData(s"client_id=$appId&client_secret=$clientSecret&code=$code")

    val tokenString = request.asString.body

    val resultJson = JSON.parseFull(tokenString)

    class CC[T] {
      def unapply(a: Option[Any]): Option[T] = if (a.isEmpty) {
        None
      } else {
        Some(a.get.asInstanceOf[T])
      }
    }
    object M extends CC[Map[String, Any]]
    object L extends CC[List[Any]]
    object S extends CC[String]
    object D extends CC[Double]
    object B extends CC[Boolean]

    Option(resultJson).flatMap { case M(map) =>
      map.get("access_token") match {
        case S(accessToken) =>
          Some("Bearer " + accessToken)
        case _ =>
          None
      }
    }
  }

}

object StravaAccess extends App {
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

  val api = new StravaAPI(appId, clientSecret, code)

  for (authString <- api.authString) {
    val athleteRequest = Http(api.stravaRoot + "athlete").header("Authorization", authString)

    val athleteString = athleteRequest.asString.body

    println(athleteString)
  }
}