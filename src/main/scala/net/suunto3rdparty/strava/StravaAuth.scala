package net.suunto3rdparty
package strava

import java.awt.Desktop
import java.io.IOException
import java.net.URL

import akka.actor.{Actor, ActorSystem, Props, ReceiveTimeout, Terminated}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import net.suunto3rdparty.moveslink.MovesLinkUploader
import net.suunto3rdparty.moveslink.MovesLinkUploader.UploadId

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.Try
import scala.xml.Elem

object StravaAuth {
  private val callbackPath = "stravaAuth.html"
  private val statusPath = "status.xml"
  private val donePath = "done.xml"
  private val saveSettingsPath = "saveSettings"
  private val deletePath = "retry"

  private val pollPeriod = 2000 // miliseconds

  private def paramPattern(param: String) = ("/.*\\?.*" + param + "=([^&\\?]*).*").r

  private val codePattern = paramPattern("code")
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

  class PollUntilTerminated extends Actor {

    def pollUntilTerminated(last: Boolean = false): Unit = {
      val timeoutDelay = pollPeriod * (if (last) 3 else 20)
      context.setReceiveTimeout(timeoutDelay.millisecond)
      println(s"pollUntilTerminated $timeoutDelay")
    }

    override def receive = {
      case ReceiveTimeout =>
        println("ReceiveTimeout")
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
    def sendResponse(code: Int, responseXml: Elem): HttpResponse = {
      val response = responseXml.toString

      val rr = HttpResponse(status = code, entity = HttpEntity(ContentTypes.`text/html(UTF-8)`, response))
      rr
    }

    def respondAuthSuccess(state: String): HttpResponse = {
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
  ajaxPost(xmlhttp, "./$saveSettingsPath?state=$state&" + pars, true); // POST to prevent caching
  return false;
}

function reupload() {
  var xmlhttp = ajax();
  ajaxPost(xmlhttp, "./$deletePath?state=$state", true); // POST to prevent caching
  return false;
}

function closingCode(){
  if (!finished) {
    var xmlhttp = ajax();
    ajaxPost(xmlhttp, "./$donePath?state=$state", false); // sync to make sure request is send before the window closes
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

      sendResponse(200, responseXml)
    }

    def respondFailure(error: String): HttpResponse = {
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

      sendResponse(400, responseXml)
    }

    def respondAuthFailure(error: String): HttpResponse = {
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

      sendResponse(400, responseXml)
    }


  }
  def statusHandler(state: String): HttpResponse = {
    if (session == state) {
      if (reportResult.nonEmpty) {

        val upload = uploadedIds.map(_.id)
        val response =
          <html>
            <h3> {reportResult} </h3>
            <p>Proceed to:
              <br/>
              <a href="https://www.strava.com">Strava</a> <br/>
              <a href="https://www.strava.com/athlete/training">My Activities</a> <br/>
              {
                val buttonGenerator = if (MovesLinkUploader.fileTest || upload.nonEmpty) List(()) else Nil
                // we need generate a sequence of Elems, may be empty depending on a condition
                buttonGenerator.map { _ =>
                  <form onSubmit="return reupload()">
                    <input type="submit" value="Delete, so that it can be uploaded again"/>
                  </form>
                }
              }
              <table>
                {upload.map { moveId =>
                  <tr>
                    <td>
                      <a href={s"https://www.strava.com/activities/$moveId"}>Move {moveId}</a>
                    </td>
                  </tr>
                }}
              </table>
            </p>
          </html>

        timeoutActor ! ServerDoneSent
        HttpHandlerHelper.sendResponse(200, response)
      } else {
        val response = <html>
          <h3>
            {reportProgress}
          </h3>
        </html>
        timeoutActor ! ServerStatusSent
        HttpHandlerHelper.sendResponse(202, response)
      }
    } else {
      val response = <error>Invalid session id</error>
      timeoutActor ! ServerStatusSent
      HttpHandlerHelper.sendResponse(400, response)

    }
  }
  def doneHandler(state: String): HttpResponse = {
    if (session == state) {
      // the session is closed, report to the server
      timeoutActor ! WindowClosedSent
    }
    HttpHandlerHelper.respondFailure("Session closed")
  }

  def saveSettingsHandler(state: String, maxHR: Option[String], questOffset: Option[String]): HttpResponse = {
    if (session == state) {

      def getIntParam(str: Option[String]) = str.flatMap(s => if (s.isEmpty) None else Some(s.toInt))

      Settings.save(getIntParam(maxHR), getIntParam(questOffset))

      HttpHandlerHelper.sendResponse(200, <html>
        <title>OK</title>
      </html>)
    } else {
      HttpHandlerHelper.respondFailure("Session closed")
    }
  }

  def deleteHandler(state: String): HttpResponse = {
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
      HttpHandlerHelper.sendResponse(200, responseXml)
    } else {
      HttpHandlerHelper.respondFailure("Session closed")
    }
  }

  def authHandler(state: String, code: Option[String], error: Option[String]): HttpResponse = {

    HttpResponse(entity = HttpEntity(
      ContentTypes.`text/html(UTF-8)`,
      "<html><body>Hello world!</body></html>"))

    if (session == "" || state == session) {
      if (code.nonEmpty) {
        session = state
        if (!authResult.isCompleted) authResult.success(code.get)
        else timeoutActor ! ServerStatusSent
        HttpHandlerHelper.respondAuthSuccess(state)
      } else if (error.nonEmpty) {
        authResult.failure(new IllegalArgumentException(s"Auth error $error"))
        HttpHandlerHelper.respondAuthFailure(error.get)
      } else {
        HttpHandlerHelper.respondAuthFailure("Unexpected response, expected code or error")
      }
    } else {
      HttpHandlerHelper.respondFailure("Session expired")
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

    val route = get {
      path(callbackPath) {
        parameters('state, 'code.?, 'error.?) { (state, code, error) =>
          complete(authHandler(state, code, error))
        }
      }
    } ~ post {
      path(statusPath) {
        parameters('state) { state =>
          complete(statusHandler(state))
        }
      } ~  path(donePath) {
        parameters('state) { state =>
          complete(doneHandler(state))
        }
      } ~  path(deletePath) {
        parameters('state) { state =>
          complete(deleteHandler(state))
        }
      } ~  path(saveSettingsPath) {
        parameters('state, 'max_hr.?, 'quest_time_offset.?) { (state, maxHR, questOffset) =>
          complete(saveSettingsHandler(state, maxHR, questOffset))
        }
      }
    }

    // `route` will be implicitly converted to `Flow` using `RouteResult.route2HandlerFlow`
    // IDEA 2016.3 currently does not follow, to prevent error highligh we use explicit handlerFlow
    val bindingFuture = Http().bindAndHandle(Route.handlerFlow(route), "localhost", 8080)

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
