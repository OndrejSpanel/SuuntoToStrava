package net.suunto3rdparty
package strava

import java.awt.Desktop
import java.io.IOException
import java.net.URL

import akka.actor.{Actor, ActorSystem, Props, ReceiveTimeout, Terminated}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import net.suunto3rdparty.moveslink.MovesLinkUploader
import net.suunto3rdparty.moveslink.MovesLinkUploader.UploadId

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.Try
import scala.xml.Elem

object StravaAuth {
  private val callbackPath = "/stravaAuth.html"
  private val statusPath = "/status.xml"
  private val donePath = "/done.xml"
  private val saveSettingsPath = "/saveSettings"
  private val deletePath = "/retry"

  private val pollPeriod = 2000 // miliseconds

  private def paramPattern(param: String) = ("/.*\\?.*" + param + "=([^&\\?]*).*").r

  private val passedPattern = paramPattern("code")
  private val errorPattern = paramPattern("error")
  private val statePattern = paramPattern("state")

  sealed trait ServerEvent
  object ServerStatusSent extends ServerEvent
  object ServerDoneSent extends ServerEvent
  object WindowClosedSent extends ServerEvent

  private val authResult = Promise[String]()

  private var reportProgress: String = "Reading files..."

  private var uploadedIds = List[UploadId]()

  private var reportResult: String = ""
  private var session: String = ""

  case object TimeoutMessage

  class PollUntilTerminated extends Actor {

    def pollUntilTerminated(last: Boolean = false): Unit = {
      val timeoutDelay = pollPeriod * (if (last) 3 else 20)
      context.setReceiveTimeout(timeoutDelay.millisecond)
      println(s"pollUntilTerminated $timeoutDelay")
    }

    override def receive = {
      case ReceiveTimeout =>
        println("TimeoutMessage")
        if (reportResult.isEmpty) {
          println("  not finished yet, giving a chance to reconnect")
          pollUntilTerminated(true)
        } else {
          println("Finished, browser closed, terminating web server")
          context.stop(self)
        }
      case ServerDoneSent =>
        println("Final status displayed.")
        pollUntilTerminated(true)
      case WindowClosedSent =>
        if (reportResult.isEmpty) {
          println("Browser window closed.")
          pollUntilTerminated(true)
        } else {
          println("Browser window closed, all done, terminating web server.")
          context.stop(self)
        }
      case ServerStatusSent => //
        println("ServerStatusSent")
        pollUntilTerminated()
      case x =>
        println(x)
        pollUntilTerminated()
    }
  }

  //noinspection FieldFromDelayedInit
  val timeoutActor = Main.system.actorOf(Props[PollUntilTerminated], "PollUntilTerminated")

  object HttpHandlerHelper {
    def sendResponse(code: Int, t: HttpRequest, responseXml: Elem): HttpResponse = {
      val response = responseXml.toString

      HttpResponse(entity = HttpEntity(ContentTypes.`text/html(UTF-8)`, response))
    }

    def respondAuthSuccess(t: HttpRequest, state: String): HttpResponse = {
      val scriptText =
      //language=JavaScript
        s"""var finished = false;

/**
 * @returns {XMLHttpRequest}
 */
function /** XMLHttpRequest */ ajax() {
  var xmlhttp;
  if (window.XMLHttpRequest) { // code for IE7+, Firefox, Chrome, Opera, Safari
    xmlhttp = new XMLHttpRequest();
  } else { // code  for IE6, IE5
    xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
  }
  return xmlhttp;
}


function updateStatus() {
  setTimeout(function () {
    var xmlhttp = ajax();
    // the callback function to be callled when AJAX request comes back
    xmlhttp.onreadystatechange = function () {
      if (xmlhttp.readyState == 4) {
        if (xmlhttp.status >= 200 && xmlhttp.status < 300) {
          var response = xmlhttp.responseXML.getElementsByTagName("html")[0];
          document.getElementById("myDiv").innerHTML = response.innerHTML;
          updateStatus(); // schedule recursively another update
        } else {
          finished = true;
          document.getElementById("myDiv").innerHTML = "<h3>Application not responding</h3>";
        }
      }
    };
    ajaxPost(xmlhttp, "./$statusPath?state=$state", true); // POST to prevent caching
  }, $pollPeriod)
}

function updateClock() {
  var d = new Date();
  document.getElementById("time").innerHTML = formatTime(d);
  document.getElementById("timeQuest").innerHTML = currentQuestTime(d);
  setTimeout(function () {
    updateClock();
  }, 1000);
}

function submitSettings(f) {
  console.log(f);
  var maxHR = f.elements["max_hr"].value;
  var questOffset = f.elements["quest_time_offset"].value;
  var pars = "max_hr=" + maxHR + "&quest_time_offset=" + questOffset;
  var xmlhttp = ajax();
  ajaxPost(xmlhttp, ".$saveSettingsPath?state=$state&" + pars, true); // POST to prevent caching
  return false;
}

function reupload() {
  var xmlhttp = ajax();
  ajaxPost(xmlhttp, ".$deletePath?state=$state", true); // POST to prevent caching
  return false;
}

function closingCode(){
  if (!finished) {
    var xmlhttp = ajax();
    ajaxPost(xmlhttp, ".$donePath?state=$state", false); // sync to make sure request is send before the window closes
    return null;
  }
}

function formatTime(d) {
  return d.toLocaleTimeString('en-US', {hour12: false});
  //var n = d.toLocaleTimeString();
}

function currentTime() {
  var d = new Date();
  return formatTime(d);
}

function currentQuestTime(d) {
  var offset = parseInt(document.getElementById("quest_time_offset").value);
  return formatTime(new Date(d.getTime() + 1000*offset));
}


function ajaxPost(/** XMLHttpRequest */ xmlhttp, /** string */ request, /** boolean */ async) {
  xmlhttp.open("POST", request, async); // POST to prevent caching
  xmlhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
  xmlhttp.send("");

}

"""

      def displaySettings: Elem = {
        <div id="settings">
          <h3>Settings:</h3>
          <form onSubmit="return submitSettings(this)">
            <table>
              <tr><td>
                Max HR</td><td><input type="number" name="max_hr" min="100" max="260" value={Settings.maxHR.toString}></input>
              </td></tr>
              <tr><td>
                Quest time offset</td><td> <input type="number" id="quest_time_offset" name="quest_time_offset" min="-60" max="60" value={Settings.questTimeOffset.toString}></input>
              </td>
                <td>Adjust up or down so that Quest time below matches the time on your watch</td>
              </tr>

              <tr>
                <td>Current time</td>
                <td id="time"></td>
              </tr>
              <tr>
                <td>Quest time</td>
                <td><b id="timeQuest"></b></td>
              </tr>
              <tr><td>
                <input type="submit" value="Save settings"/>
              </td></tr>
            </table>
          </form>
        </div>
      }

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

          {displaySettings}

          <div id="myDiv">
            <h3>Starting processing...</h3>
          </div>

        </body>
        <script>
          updateStatus()
          updateClock()
          window.onbeforeunload = closingCode;
        </script>
      </html>

      sendResponse(200, t, responseXml)
    }

    def respondFailure(t: HttpRequest, error: String): HttpResponse = {
      val responseXml =
        <html>
          <title>Suunto To Strava Authentication</title>
          <body>
            <h1>Suunto To Strava Authenticated</h1>
            <p>This window has expired.<br/>You may have opened another window?<br/>
              Error: {error}
            </p>
            <p>Proceed to:
              <br/>
              <a href="https://www.strava.com">Strava</a> <br/>
            </p>
          </body>
        </html>

      sendResponse(400, t, responseXml)
    }

    def respondAuthFailure(t: HttpRequest, error: String): HttpResponse = {
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
  def statusHandler(r: HttpRequest): HttpResponse = {
    val requestURL = r.uri.toString
    println(requestURL)
    val state = requestURL match {
      case statePattern(s) => s
      case _ => ""
    }
    if (session == state) {
      if (reportResult.nonEmpty) {

        val upload = uploadedIds.map(_.id)
        val response =
          <html>
            <h3>
              {reportResult}
            </h3>
            <p>Proceed to:
              <br/>
              <a href="https://www.strava.com">Strava</a> <br/>
              <a href="https://www.strava.com/athlete/training">My Activities</a> <br/>{val buttonGenerator = if (MovesLinkUploader.fileTest || upload.nonEmpty) List(()) else Nil
            // we need generate a sequence of Elems, may be empty depending on a condition
            buttonGenerator.map { _ =>
              <form onSubmit="return reupload()">
                <input type="submit" value="Delete, so that it can be uploaded again"/>
              </form>
            }}<table>
              {upload.map { moveId =>
                <tr>
                  <td>
                    <a href={s"https://www.strava.com/activities/$moveId"}>Move
                      {moveId}
                    </a>
                  </td>
                </tr>
              }}
            </table>
            </p>
          </html>

        timeoutActor ! ServerDoneSent
        HttpHandlerHelper.sendResponse(200, r, response)
      } else {
        val response = <html>
          <h3>
            {reportProgress}
          </h3>
        </html>
        timeoutActor ! ServerStatusSent
        HttpHandlerHelper.sendResponse(202, r, response)
      }
    } else {
      val response = <error>Invalid session id</error>
      timeoutActor ! ServerStatusSent
      HttpHandlerHelper.sendResponse(400, r, response)

    }
  }
  def doneHandler(t: HttpRequest): HttpResponse = {
    val requestURL = t.uri.toString
    println(requestURL)
    val state = requestURL match {
      case statePattern(s) => s
      case _ => ""
    }
    if (session == state) {
      // the session is closed, report to the server
      timeoutActor ! WindowClosedSent
    }
    HttpHandlerHelper.respondFailure(t, "Session closed")
  }

  def saveSettingsHandler(t: HttpRequest): HttpResponse = {
    val requestURL = t.uri.toString
    val state = requestURL match {
      case statePattern(s) => s
      case _ => ""
    }
    if (session == state) {
      val maxHRPattern = paramPattern("max_hr")
      val questOffsetPattern = paramPattern("quest_time_offset")
      val maxHR = requestURL match {
        case maxHRPattern(s) => Some(s.toInt)
        case _ => None
      }
      val questOffset = requestURL match {
        case questOffsetPattern(s) => Some(s.toInt)
        case _ => None
      }

      Settings.save(maxHR, questOffset)

      HttpHandlerHelper.sendResponse(200, t, <html>
        <title>OK</title>
      </html>)
    } else {
      HttpHandlerHelper.respondFailure(t, "Session closed")
    }
  }

  def deleteHandler(t: HttpRequest): HttpResponse = {
    val requestURL = t.uri.toString
    println(requestURL)
    val state = requestURL match {
      case statePattern(s) => s
      case _ => ""
    }
    if (session == state) {
      uploadedIds.foreach { id =>
        id.filenames.foreach(MovesLinkUploader.unmarkUploadedFile)

        //noinspection FieldFromDelayedInit
        Main.api.deleteActivity(id.id)
      }

      Main.startUpload()

      val responseXml = <html>
        <title>Deleted</title> <body>All files deleted.</body>
      </html>
      HttpHandlerHelper.sendResponse(200, t, responseXml)
    } else {
      HttpHandlerHelper.respondFailure(t, "Session closed")
    }
  }

  def authHandler(r: HttpRequest): HttpResponse = {

    HttpResponse(entity = HttpEntity(
      ContentTypes.`text/html(UTF-8)`,
      "<html><body>Hello world!</body></html>"))

    // Url expected in form: /stravaAuth.html?state=&code=xxxxxxxx
    val requestURL = r.uri.toString // TODO: better parsing using akka http means
    println(requestURL)
    val state = requestURL match {
      case statePattern(s) => s
      case _ => ""
    }
    if (session == "" || session == state) {
      requestURL match {
        case passedPattern(code) =>
          session = state
          if (!authResult.isCompleted) authResult.success(code)
          else timeoutActor ! ServerStatusSent
          HttpHandlerHelper.respondAuthSuccess(r, state)
        case errorPattern(error) =>
          authResult.failure(new IllegalArgumentException(s"Unexpected URL $requestURL"))
          HttpHandlerHelper.respondAuthFailure(r, error)
        case _ =>
          authResult.failure(new IllegalArgumentException(s"Unexpected URL $requestURL"))
          HttpHandlerHelper.respondAuthFailure(r, "Unknown error")
      }
    } else {
      HttpHandlerHelper.respondFailure(r, "Session expired")
    }

  }

  class TimeoutTerminator(server: ServerShutdown) extends Actor {

    context.watch(timeoutActor)

    def receive = {
      case Terminated(_) =>
        println("Poll actor terminated")
        // we do not need a CountDownLatch, as Await on the promise makes sure the response serving has already started
        stopServer(server)
        context.stop(self)
        context.system.terminate()
        println("Terminated actor system")
    }
  }

  case class ServerShutdown(binding: Future[ServerBinding], system: ActorSystem)

  // http://stackoverflow.com/a/3732328/16673
  private def startHttpServer(callbackPort: Int): ServerShutdown = {

    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    // needed for the future map/flatmap in the end

    val requestHandler: HttpRequest => HttpResponse = {
      case r@HttpRequest(GET, Uri.Path(`callbackPath`), _, _, _) =>
        authHandler(r)

      case r@HttpRequest(GET, Uri.Path(`statusPath`), _, _, _) =>
        statusHandler(r)

      case r@HttpRequest(GET, Uri.Path(`statusPath`), _, _, _) =>
        doneHandler(r)

      case r@HttpRequest(GET, Uri.Path(`saveSettingsPath`), _, _, _) =>
        saveSettingsHandler(r)

      case r@HttpRequest(GET, Uri.Path(`deletePath`), _, _, _) =>
        deleteHandler(r)

      case r: HttpRequest =>
        r.discardEntityBytes() // important to drain incoming HTTP Entity stream
        HttpResponse(404, entity = "Unknown resource!")
    }

    val bindingFuture = Http().bindAndHandleSync(requestHandler, "localhost", 8080)

    val server = ServerShutdown(bindingFuture, system)

    //noinspection FieldFromDelayedInit
    Main.system.actorOf(Props(classOf[TimeoutTerminator], server), "TimeoutTerminator")

    server

  }

  def stopServer(server: ServerShutdown): Unit = {
    implicit val executionContext = server.system.dispatcher
    server.binding
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => server.system.terminate()) // and shutdown when done
  }

  def apply(appId: Int, callbackPort: Int, access: String): Option[String] = {

    startHttpServer(callbackPort)

    val sessionId = System.currentTimeMillis().toHexString
    val callbackUrl = s"http://localhost:$callbackPort/$callbackPath"
    val forcePrompt = false // useful for debugging / troubleshooting
    val forceStr = if (forcePrompt) "&approval_prompt=force" else ""
    val url = s"https://www.strava.com/oauth/authorize?client_id=$appId&scope=$access&response_type=code&redirect_uri=$callbackUrl&state=$sessionId$forceStr"
    try {
      Desktop.getDesktop.browse(new URL(url).toURI)
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }

    val ret = Try (Await.result(authResult.future, 5.minutes)).toOption

    timeoutActor ! ServerStatusSent

    ret
  }

  def progress(status: String): Unit = {
    println(s"Progress: $status")
    reportProgress = status
  }

  def stop(status: String, uploaded: List[UploadId]): Unit = {
    reportResult = status

    uploadedIds = uploaded
  }

}
