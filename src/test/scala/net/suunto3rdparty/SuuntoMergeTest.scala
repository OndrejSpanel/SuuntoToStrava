package net.suunto3rdparty

import java.time.format.DateTimeFormatter

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try
import scala.xml._

class SuuntoMergeTest extends FlatSpec with Matchers {
  behavior of "SuuntoMerge"

  private def gpsPodMove = {
    val res = getClass.getResourceAsStream("/suuntoMerge/Moveslink2/gps.sml")

    val doc = moveslink2.XMLParser.getDeviceLog(XML.load(res))
    val move = moveslink2.XMLParser.parseXML("gps.sml", doc)
    move
  }

  private def questMove = {
    val res = getClass.getResourceAsStream("/suuntoMerge/Moveslink/quest.xml")
    val doc = XML.load(res)

    val move = moveslink.XMLParser.parseXML("quest.xml", doc)
    move
  }

  it should "load Quest file" in {
    val move = questMove

    move.isEmpty shouldBe false

    move.foreach { m =>
      val hr = m.streamGet[DataStreamHRWithDist]
      hr.isEmpty shouldBe false

      m.streamGet[DataStreamLap].isEmpty shouldBe false

      val t = DateTimeFormatter.ISO_DATE_TIME.parse("2016-10-21T06:46:57Z")
      m.startTime.contains(t)
      m.duration shouldBe 842.4
    }
  }

  it should "load GPS pod file" in {
    val move = gpsPodMove

    move.isFailure shouldBe false

    move.foreach { m =>
      val gps = m.streamGet[DataStreamGPS]
      gps.isEmpty shouldBe false

      val t = DateTimeFormatter.ISO_DATE_TIME.parse("2016-10-21T06:46:01Z")
      m.startTime.contains(t)
      m.duration shouldBe 4664.6


    }

  }

  it should "merge GPS + Quest files" in {
    for (hr <- questMove; gps <- gpsPodMove) {
      val m = gps.addStream(hr, hr.stream[DataStreamHRWithDist])
      m.isEmpty shouldBe false
      m.duration shouldBe 4664.6
      m.isAlmostEmpty(30) shouldBe false

    }
  }

}
