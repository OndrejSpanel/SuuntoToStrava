package net.suunto3rdparty
package tcx

import java.io._
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import MoveHeader.ActivityType._
import Util._
import resource._

import scala.collection.immutable.SortedMap
import scala.xml.{Node, NodeSeq}

object Export {

  def writeLaps(move: Move): NodeSeq = {
    val timeBeg = move.startTime.get
    val timeEnd = move.endTime.get

    val timeFormat = DateTimeFormatter.ISO_DATE_TIME
    val timeBegText = timeBeg.format(timeFormat)

    type MultiSamples = SortedMap[ZonedDateTime, Seq[DataStream#Item]]

    def toMultiSamples(data: DataStream#DataMap): MultiSamples = {
      data.mapValues(Seq(_))
    }

    def mergeMultiSamples(m: MultiSamples, d: DataStream#DataMap): MultiSamples = {
      val updateExisting = m.map { case (k, v) =>
        k -> (v ++ d.get(k))
      }
      val addNew = d.filterKeys(k => !m.contains(k)).mapValues(Seq(_))
      SortedMap(updateExisting.toSeq: _*) ++ addNew
    }

    // multiple items may share the same timestamp
    val combined = move.streams.tail.foldLeft(toMultiSamples(move.streams.head._2.stream)) { (comb, str) =>
      mergeMultiSamples(comb, str._2.stream)
    }

    def writeEvent(ev: DataStream#Item): NodeSeq = {
      ev match {
        case gps: GPSPoint =>
            <Position>
              <LatitudeDegrees>{gps.latitude}</LatitudeDegrees>
              <LongitudeDegrees>{gps.longitude}</LongitudeDegrees>
            </Position>

        case hr: HRPoint =>
          /*
          val myMsg = new RecordMesg()
          if (hr.hr != 0) {
            myMsg.setTimestamp(toTimestamp(time))
            myMsg.setHeartRate(hr.hr.toShort)
            myMsg.setDistance(hr.dist.toFloat)
            Some(myMsg)
          } else None
          */
          NodeSeq.Empty
        case hr: Int =>
          /*
          val myMsg = new RecordMesg()
          myMsg.setTimestamp(toTimestamp(time))
          myMsg.setHeartRate(hr.toShort)
          Some(myMsg)
          */
          NodeSeq.Empty
        case dist: Double =>
          /*
          val myMsg = new RecordMesg()
          myMsg.setTimestamp(toTimestamp(time))
          myMsg.setDistance(dist.toFloat)
          Some(myMsg)
          */
          NodeSeq.Empty
        case lap: String =>
          NodeSeq.Empty
      }
    }
    def writeEventGroup(evGroup: (ZonedDateTime, Seq[DataStream#Item])): Node = {
      <Trackpoint>
      {
        for (ev <- evGroup._2) yield {
          <Time>{evGroup._1.format(timeFormat)}</Time> +: {writeEvent(ev)}
        }
      }
      </Trackpoint>
    }

    def writeLap(lapBeg: ZonedDateTime, lapEnd: ZonedDateTime): NodeSeq = {
      <Track>{
      val lapData = combined.dropWhile(_._1 < lapBeg).takeWhile(_._1 < lapEnd)

      val events = for (evGroup <- lapData) yield {
        writeEventGroup(evGroup)
      }
      events.toSeq}
      </Track>
    }

    //noinspection MapKeys - need to keep order
    // detect laps and write them
    val lapBoundaries = move.streams.get(StreamLap).map(_.asInstanceOf[DataStreamLap].stream.map(_._1).toSeq).getOrElse(Seq(timeBeg)) :+ timeEnd

    val lapsWithDuration = (lapBoundaries zip lapBoundaries.tail).map { case (lapBeg, lapEnd) =>
      (lapBeg, lapEnd)
    }

    <Id>{timeBegText}</Id> +: {
      for ((lapBeg, lapEnd) <- lapsWithDuration) yield {
        <Lap StartTime={lapBeg.format(timeFormat)}>
          <TotalTimeSeconds>{timeDifference(lapBeg, lapEnd)}</TotalTimeSeconds>
          {writeLap(lapBeg, lapEnd)}
        </Lap>
      }
    }
  }

  def toOutputStream(os: OutputStream, move: Move) {
    val sport = move.header.moveType match {
      case RunningTrail => "Running"
      case RunningRoad => "Running"
      case Orienteering => "Running"
      case MountainBike => "Biking"
      case Cycling => "Biking"
      case Unknown => "Running"
    }

    val doc = <TrainingCenterDatabase xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2">
      <Activities>
        <Activity Sport={sport}>
          {writeLaps(move: Move)}
        </Activity>
      </Activities>
    </TrainingCenterDatabase>

    for (writer <- managed(new OutputStreamWriter(os))) {
      scala.xml.XML.write(writer, doc, "UTF-8", xmlDecl = true, null)
    }
  }

  def apply(move: Move): Unit = {
    for (time <- move.streams.head._2.startTime) {
      val file = new File("testoutput_" + time.toFileName + ".tcx")

      for (fos <- managed(new FileOutputStream(file))) {
        toOutputStream(fos, move)
      }
    }

  }

}
