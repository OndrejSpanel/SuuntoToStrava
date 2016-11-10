package net.suunto3rdparty

import java.time.format.DateTimeFormatter

import org.scalatest.{FlatSpec, Matchers}

import scala.xml._

class SuuntoMergeTest extends FlatSpec with Matchers {
  behavior of "SuuntoMerge"

  private def questStream = getClass.getResourceAsStream("/suuntoMerge/Moveslink/quest.xml")
  private def gpsStream = getClass.getResourceAsStream("/suuntoMerge/Moveslink2/gps.sml")

  it should "load Quest file" in {
    val res = questStream
    val doc = XML.load(res)

    val move = moveslink.XMLParser.parseXML("quest.xml", doc)

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
    val res = gpsStream

  }

  it should "merge GPS + Quest files" in {

  }

}
