package net.suunto3rdparty.strava

import java.awt.Desktop
import java.io.IOException
import java.net.URL

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.util.Try

object StravaAuth {

  val authResult = Promise[String]()

  def apply(appId: Int, callbackUrl: String): String = {
    val url = s"https://www.strava.com/oauth/authorize?client_id=$appId&response_type=code&redirect_uri=$callbackUrl"
    try {
      Desktop.getDesktop.browse(new URL(url).toURI)
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }

    Await.result(authResult.future, Duration.Inf)
  }

}
