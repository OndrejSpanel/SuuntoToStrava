package net.suunto3rdparty.strava

import java.awt.Desktop
import java.io.IOException
import java.net.{InetSocketAddress, URL}

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}

object StravaAuth {

  val authResult = Promise[String]()

  object HttpHandler extends HttpHandler {

    def handle(t: HttpExchange): Unit = {
      val requestUrl = t.getRequestURI
      // Url expected in form: /stravaAuth.html?state=&code=xxxxxxxx
      val Pattern = "/.*?.*&code=([a-z0-9]*)".r

      requestUrl.toASCIIString match {
        case Pattern(code) =>
          authResult.success(code)
        case _ =>
          authResult.failure(new IllegalArgumentException(s"Unexpected URL $requestUrl"))
      }

      val responseXml =
        <html>
          <title>Suunto To Strava Authenticated</title>
          <body>
            <h1>Suunto To Strava Authenticated</h1>
            <p>Suunto To Strava automated upload application authenticated to Strava</p>
          </body>
        </html>

      val response = responseXml.toString

      t.sendResponseHeaders(200, response.length)
      val os = t.getResponseBody
      os.write(response.getBytes)
      os.close()
    }
  }

  // http://stackoverflow.com/a/3732328/16673
  def startHttpServer(callbackPort: Int, callbackPath: String) = {
    val server = HttpServer.create(new InetSocketAddress(8080), 0)
    server.createContext(s"/$callbackPath", HttpHandler)
    server.setExecutor(null) // creates a default executor
    server.start()
  }

  def apply(appId: Int, callbackPort: Int, callbackPath: String, access: String): String = {
    startHttpServer(callbackPort, callbackPath)

    val callbackUrl = s"http://localhost:$callbackPort/$callbackPath"
    val url = s"https://www.strava.com/oauth/authorize?client_id=$appId&scope=$access&response_type=code&redirect_uri=$callbackUrl"
    try {
      Desktop.getDesktop.browse(new URL(url).toURI)
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }

    Await.result(authResult.future, Duration.Inf)
  }

}
