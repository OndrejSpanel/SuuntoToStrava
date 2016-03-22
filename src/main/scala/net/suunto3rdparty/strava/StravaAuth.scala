package net.suunto3rdparty.strava

import java.awt.Desktop
import java.io.IOException
import java.net.{InetSocketAddress, URL}
import java.util.concurrent._

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import scala.util.Try
import scala.xml.Elem

object StravaAuth {
  private val callbackPath = "stravaAuth.html"
  private val statusPath = "status.html"
  private val pollPeriod = 2000 // miliseconds

  sealed trait ServerEvent
  object ServerStatusSent extends ServerEvent
  object ServerDoneSent extends ServerEvent


  private case class ServerShutdown(server: HttpServer, executor: ExecutorService, events: LinkedBlockingQueue[ServerEvent])
  private val authResult = Promise[String]()
  private var server: Option[ServerShutdown] = None

  val timeoutThread = new Thread(new Runnable {
    override def run(): Unit = {
      @tailrec
      def pollUntilTerminated(): Unit = {
        val event = try {
          server.flatMap(s => Option(s.events.poll(pollPeriod * 5, TimeUnit.MILLISECONDS)))
        } catch {
          case _: InterruptedException =>
            return
        }
        if (event.nonEmpty && event.get != ServerDoneSent) {
          pollUntilTerminated()
        } else {
          println("Browser closed? Status not polled, timeout")
        }
      }

      pollUntilTerminated()
    }
  })


  var reportProgress: String = "Processing and uploading..."
  var reportResult: String = ""

  abstract class HttpHandlerHelper extends HttpHandler {
    protected def sendResponse(code: Int, t: HttpExchange, responseXml: Elem): Unit = {
      val response = responseXml.toString

      t.sendResponseHeaders(code, response.length)
      val os = t.getResponseBody
      os.write(response.getBytes)
      os.close()
    }

  }
  object StatusHandler extends HttpHandlerHelper {
    override def handle(httpExchange: HttpExchange): Unit = {
      if (reportResult.nonEmpty) {
        val response =
          <div>
            <h3>{reportResult}</h3>
            <p>Proceed to:
              <br/>
              <a href="https://www.strava.com">Strava</a> <br/>
              <a href="https://www.strava.com/athlete/training">My Activities</a>
            </p>
          </div>

        sendResponse(200, httpExchange, response)
        server.foreach(_.events.put(ServerDoneSent))
      } else {
        val response = <h3>{reportProgress}</h3>
        sendResponse(202, httpExchange, response)
        server.foreach(_.events.put(ServerStatusSent))
      }
    }
  }

  object AuthHandler extends HttpHandlerHelper {

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
      val scriptText =
      //language=JavaScript
s"""
function updateStatus() {
  setTimeout(function(){
    var xmlhttp;
    if (window.XMLHttpRequest) { // code for IE7+, Firefox, Chrome, Opera, Safari
      xmlhttp=new XMLHttpRequest();
    }
    else { // code for IE6, IE5
      xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
    }
    // the callback function to be callled when AJAX request comes back
    xmlhttp.onreadystatechange=function(){
      if (xmlhttp.readyState==4) {
        if(xmlhttp.status>=200 && xmlhttp.status<300){
          document.getElementById("myDiv").innerHTML=xmlhttp.responseText;
          if (xmlhttp.status==202){
           updateStatus() // schedule recursively another update
          }
        } else {
          document.getElementById("myDiv").innerHTML="<h3>Application not responding</h3>";
        }
      }
    };
    xmlhttp.open("POST","./$statusPath",true); // POST to prevent caching
    xmlhttp.setRequestHeader("Content-type","application/x-www-form-urlencoded");
    xmlhttp.send("");
  }, $pollPeriod)
}
"""

      val responseXml = <html>
        <head>
          <script type="text/javascript">
            {scala.xml.Unparsed(scriptText)}
          </script>
        </head>

        <title>Suunto To Strava Authentication</title>
        <body>
          <h1>Suunto To Strava Authenticated</h1>
          <p>Suunto To Strava automated upload application authenticated to Strava</p>

          <div id="myDiv">
            <h3>Starting processing...</h3>
          </div>

        </body>
        <script>updateStatus()</script>
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

  }

  // http://stackoverflow.com/a/3732328/16673
  private def startHttpServer(callbackPort: Int) = {
    val ex = Executors.newSingleThreadExecutor()
    val events = new LinkedBlockingQueue[ServerEvent]()

    val server = HttpServer.create(new InetSocketAddress(8080), 0)
    server.createContext(s"/$callbackPath", AuthHandler)
    server.createContext(s"/$statusPath", StatusHandler)

    server.setExecutor(ex) // creates a default executor
    server.start()
    ServerShutdown(server, ex, events)
  }

  def apply(appId: Int, callbackPort: Int, access: String): Option[String] = {
    server = Some(startHttpServer(callbackPort))

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

    val ret = Try (Await.result(authResult.future, Duration(5, TimeUnit.MINUTES))).toOption

    timeoutThread.start()

    ret
  }

  def progress(status: String): Unit = {
    reportProgress = status
  }

  def stop(status: String): Unit = {
    reportResult = status
    server.foreach { s =>
      // based on http://stackoverflow.com/a/36129257/16673
      timeoutThread.join()
      // we do not need a CountDownLatch, as Await on the promise makes sure the response serving has already started
      s.executor.shutdown()
      s.executor.awaitTermination(1, TimeUnit.MINUTES); // wait until all tasks complete (i. e. all responses are sent)
      s.server.stop(0)
    }
  }

}
