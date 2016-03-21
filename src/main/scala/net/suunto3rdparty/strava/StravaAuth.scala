package net.suunto3rdparty.strava

import java.awt.Desktop
import java.io.IOException
import java.net.{InetSocketAddress, URL}
import java.util.concurrent._

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}

object StravaAuth {

  import java.util.concurrent.ThreadPoolExecutor
  import java.util.concurrent.TimeUnit

  class MyThreadPoolExecutor extends ThreadPoolExecutor(1, 2, 1, TimeUnit.SECONDS, new LinkedBlockingQueue[Runnable]())

  val authResult = Promise[String]()
  case class ServerShutdown(server: HttpServer, executor: ExecutorService, latch: CountDownLatch)
  var server: Option[ServerShutdown] = None

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
            <p>Proceed to:<br/>
              <a href="https://www.strava.com">Strava</a><br/>
              <a href="https://www.strava.com/athlete/training">My Activities</a>
            </p>
          </body>
        </html>

      val response = responseXml.toString

      t.sendResponseHeaders(200, response.length)
      val os = t.getResponseBody
      os.write(response.getBytes)
      os.close()
      server.foreach(s => s.latch.countDown())
      //server.foreach(_.stop(5))
    }
  }

  // http://stackoverflow.com/a/3732328/16673
  def startHttpServer(callbackPort: Int, callbackPath: String) = {
    val ex = Executors.newSingleThreadExecutor()
    val c = new CountDownLatch(1)

    val server = HttpServer.create(new InetSocketAddress(8080), 0)
    server.createContext(s"/$callbackPath", HttpHandler)

    server.setExecutor(ex) // creates a default executor
    server.start()
    ServerShutdown(server, ex, c)
  }

  def apply(appId: Int, callbackPort: Int, callbackPath: String, access: String): String = {
    server = Some(startHttpServer(callbackPort, callbackPath))

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

  def stop(): Unit = {
    server.foreach { s =>
      s.latch.await()
      s.executor.shutdown()
      s.executor.awaitTermination(1, TimeUnit.MINUTES); // wait until all tasks complete (i. e. all responses are sent)
      s.server.stop(0)
    }
  }

}
