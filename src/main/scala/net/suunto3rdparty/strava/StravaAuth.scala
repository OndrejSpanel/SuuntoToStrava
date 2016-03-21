package net.suunto3rdparty.strava

import java.awt.Desktop
import java.io.IOException
import java.net.{InetSocketAddress, URL}
import java.util.concurrent._

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import scala.util.Try
import scala.xml.Elem

object StravaAuth {


  val authResult = Promise[String]()
  case class ServerShutdown(server: HttpServer, executor: ExecutorService)
  var server: Option[ServerShutdown] = None

  object HttpHandler extends HttpHandler {

    def handle(t: HttpExchange): Unit = {
      val requestUrl = t.getRequestURI
      // Url expected in form: /stravaAuth.html?state=&code=xxxxxxxx
      val passedPattern = "/.*?.*&code=([^&?]*)".r
      val errorPattern = "/.*?.*&error=([^&?]*)".r

      requestUrl.toASCIIString match {
        case passedPattern(code) =>
          respondAuthSuccess(t)
          authResult.success(code)
        case errorPattern(error) =>
          respondAuthFailure(t, error)
          authResult.failure(new IllegalArgumentException(s"Unexpected URL $requestUrl"))
        case _ =>
          respondAuthFailure(t, "Unknown error")
          authResult.failure(new IllegalArgumentException(s"Unexpected URL $requestUrl"))
      }

    }

    private def respondAuthSuccess(t: HttpExchange): Unit = {
      val responseXml =
        <html>
          <title>Suunto To Strava Authentication</title>
          <body>
            <h1>Suunto To Strava Authenticated</h1>
            <p>Suunto To Strava automated upload application authenticated to Strava</p>
            <p>Proceed to:
              <br/>
              <a href="https://www.strava.com">Strava</a> <br/>
              <a href="https://www.strava.com/athlete/training">My Activities</a>
            </p>
          </body>
        </html>

      sendResponse(200, t, responseXml)
    }

    private def respondAuthFailure(t: HttpExchange, error: String): Unit = {
      val responseXml =
        <html>
          <title>Suunto To Strava Authentication</title>
          <body>
            <h1>Suunto To Strava Not Authenticated</h1>
            <p>Suunto To Strava automated upload application not authenticated to Strava.<br/>
              Error: {error}
            </p>
            <p>Proceed to:
              <br/>
              <a href="https://www.strava.com">Strava</a> <br/>
              <a href="https://www.strava.com/settings/apps">Check Strava apps settings</a>
            </p>
          </body>
        </html>

      sendResponse(400, t, responseXml)
    }

    private def sendResponse(code: Int, t: HttpExchange, responseXml: Elem): Unit = {
      val response = responseXml.toString

      t.sendResponseHeaders(code, response.length)
      val os = t.getResponseBody
      os.write(response.getBytes)
      os.close()
    }
  }

  // http://stackoverflow.com/a/3732328/16673
  def startHttpServer(callbackPort: Int, callbackPath: String) = {
    val ex = Executors.newSingleThreadExecutor()

    val server = HttpServer.create(new InetSocketAddress(8080), 0)
    server.createContext(s"/$callbackPath", HttpHandler)

    server.setExecutor(ex) // creates a default executor
    server.start()
    ServerShutdown(server, ex)
  }

  def apply(appId: Int, callbackPort: Int, callbackPath: String, access: String): Option[String] = {
    server = Some(startHttpServer(callbackPort, callbackPath))

    val callbackUrl = s"http://localhost:$callbackPort/$callbackPath"
    val forcePrompt = false // useful for debugging / troubleshooting
    val forceStr = if (forcePrompt) "&approval_prompt=force" else ""
    val url = s"https://www.strava.com/oauth/authorize?client_id=$appId&scope=$access&response_type=code&redirect_uri=$callbackUrl$forceStr"
    try {
      Desktop.getDesktop.browse(new URL(url).toURI)
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }

    Try (Await.result(authResult.future, Duration(5, TimeUnit.MINUTES))).toOption
  }

  def stop(): Unit = {
    server.foreach { s =>
      // based on http://stackoverflow.com/a/36129257/16673
      // we do not need a CountDownLatch, as Await on the promise makes sure the response serving has already started
      s.executor.shutdown()
      s.executor.awaitTermination(1, TimeUnit.MINUTES); // wait until all tasks complete (i. e. all responses are sent)
      s.server.stop(0)
    }
  }

}
