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
          <HeartRateBpm><Value>{hr.hr}</Value></HeartRateBpm>
          <DistanceMeters>{hr.dist}</DistanceMeters>
        case hr: Int =>
          <HeartRateBpm><Value>{hr}</Value></HeartRateBpm>
        case dist: Double =>
          <DistanceMeters>{dist}</DistanceMeters>
        case lap: String =>
          NodeSeq.Empty
      }
    }
    def writeEventGroup(evGroup: (ZonedDateTime, Seq[DataStream#Item])): NodeSeq = {
      val events = <Time>{evGroup._1.format(timeFormat)}</Time> +: evGroup._2.map(writeEvent)
      if (events.nonEmpty) {
        <Trackpoint>
          {events}
        </Trackpoint>
      } else Nil
    }

    def writeLap(lapBeg: ZonedDateTime, lapEnd: ZonedDateTime): Node = {
      val lapData = combined.dropWhile(_._1 < lapBeg).takeWhile(_._1 < lapEnd)
      val distOnly = lapData.map(_._2.collect {
        case d: Double => d
        case HRPoint(_, d) => d
      } ).filter(_.nonEmpty)
      val dist = for (begDist <- distOnly.headOption; endDist <- distOnly.lastOption) yield endDist.head - begDist.head
      <Lap StartTime={lapBeg.format(timeFormat)}>
        {
        <TotalTimeSeconds>{timeDifference(lapBeg, lapEnd)}</TotalTimeSeconds> +:
        dist.toSeq.map(d => <DistanceMeters>{d}</DistanceMeters>) :+
        <Track>
          {lapData.flatMap(writeEventGroup).toSeq}
        </Track>
        }
      </Lap>
    }

    //noinspection MapKeys - need to keep order
    // detect laps and write them
    val lapBoundariesAll = timeBeg +: move.streamGet[DataStreamLap].map(_.stream.map(_._1).toSeq).getOrElse(Seq(timeBeg)) :+ timeEnd

    val lapBoundaries = lapBoundariesAll.distinct

    val lapsWithDuration = lapBoundaries zip lapBoundaries.tail

    <Id>{timeBegText}</Id> +: lapsWithDuration.map((writeLap _).tupled)
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

    def writeCreator: Option[Node] = {
      val deviceName = MoveHeader.mergeDeviceNames(move.header.deviceNames)
      deviceName.map { move =>
        <Creator xsi:type="Device_t">
          <Name>{move}</Name>
          <UnitId>0</UnitId>
          <ProductID>0</ProductID>
        </Creator>
      }
    }
    val doc = <TrainingCenterDatabase xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2">
      <Activities>
        {
          <Activity Sport={sport}>
          {
            writeLaps(move: Move) ++ writeCreator.toSeq
          }
          </Activity>
        }
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
