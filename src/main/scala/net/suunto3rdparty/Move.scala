package net.suunto3rdparty

import java.time.ZonedDateTime

import Util._

import scala.collection.immutable.SortedMap

case class Header(startTime: ZonedDateTime = ZonedDateTime.now, durationMs: Int = 0, calories: Int = 0, distance: Int = 0)
case class MoveHeader(moveType: String = "Run")

case class GPSPoint(latitude: Double, longitude: Double, elevation: Option[Int])
case class HRPoint(hr: Int, dist: Double)

sealed trait StreamType
object StreamGPS extends StreamType {
  override def toString: String = "GPS"
}
object StreamHR extends StreamType {
  override def toString: String = "HR"
}
object StreamDist extends StreamType {
  override def toString: String = "Dist"
}
object StreamHRWithDist extends StreamType {
  override def toString: String = "HR_Dist"
}

sealed abstract class DataStream(val streamType: StreamType) {

  type Item

  type DataMap = SortedMap[ZonedDateTime, Item]

  def stream: DataMap

  def pickData(data: DataMap): DataStream

  val startTime: Option[ZonedDateTime] = stream.headOption.map(_._1)
  val endTime: Option[ZonedDateTime] = stream.lastOption.map(_._1)

  def takeUntil(time: ZonedDateTime): (DataStream, DataStream) = {
    val (take, left) = stream.span(_._1 < time)
    (pickData(take), pickData(left))
  }

  def toLog = s"$streamType: ${startTime.map(_.toLog)} .. ${endTime.map(_.toLogShort)}"
  override def toString = toLog

}

class DataStreamGPS(override val stream: SortedMap[ZonedDateTime, GPSPoint]) extends DataStream(StreamGPS) {
  type Item = GPSPoint

  override def pickData(data: DataMap) = new DataStreamGPS(data)
}

class DataStreamHRWithDist(override val stream: SortedMap[ZonedDateTime, HRPoint]) extends DataStream(StreamHRWithDist) {
  type Item = HRPoint

  def rebase: DataStream = {
    if (stream.isEmpty) this
    else {
      val base = stream.head._2.dist
      new DataStreamHRWithDist(stream.mapValues(v => v.copy(dist = v.dist  - base)))
    }
  }

  override def pickData(data: DataMap) = new DataStreamHRWithDist(data).rebase
}

class DataStreamHR(override val stream: SortedMap[ZonedDateTime, Int]) extends DataStream(StreamHR) {
  type Item = Int

  override def pickData(data: DataMap) = new DataStreamHR(data)
}

class DataStreamDist(override val stream: SortedMap[ZonedDateTime, Double]) extends DataStream(StreamDist) {

  type Item = Double

  def rebase: DataStream = {
    if (stream.isEmpty) this
    else {
      val base = stream.head._2
      new DataStreamDist(stream.mapValues(_ - base))
    }
  }

  override def pickData(data: DataMap) = new DataStreamDist(data).rebase
}


object Move {
  implicit def ordering: Ordering[Move] = {
    new Ordering[Move] {
      override def compare(x: Move, y: Move) = {
        (x.startTime, y.startTime) match {
          case (Some(xt), Some(yt)) => xt compareTo yt
          case (None, None) => 0
          case (None, Some(yt)) => -1
          case (Some(xt), None) => +1
        }
      }
    }
  }
}

case class Move(header: MoveHeader, streams: Map[StreamType, DataStream]) {

  def this(header: MoveHeader, streamSeq: DataStream*) = {
    this(header, streamSeq.map(s => s.streamType -> s).toMap)
  }

  private def startTimeOfStreams(ss: Iterable[DataStream]) = ss.flatMap(_.startTime).minOpt
  private def endTimeOfStreams(ss: Iterable[DataStream]) = ss.flatMap(_.endTime).maxOpt

  val startTime: Option[ZonedDateTime] = startTimeOfStreams(streams.values)
  val endTime: Option[ZonedDateTime] = endTimeOfStreams(streams.values)

  def isEmpty = startTime.isEmpty
  def isAlmostEmpty(minDurationSec: Long) = !streams.exists(_._2.stream.nonEmpty) || endTime.get < startTime.get.plusSeconds(minDurationSec)

  def toLog: String = streams.mapValues(_.toLog).mkString(", ")

  def addStream(stream: DataStream) = copy(streams = streams + (stream.streamType -> stream))

  def takeUntil(time: ZonedDateTime): (Option[Move], Option[Move]) = {
    val split = streams.mapValues(_.takeUntil(time))

    val take = split.mapValues(_._1)
    val left = split.mapValues(_._2)

    val takeMove = copy(streams = take)
    val leftMove = copy(streams = left)
    (takeMove.endTime.map(_ => takeMove), leftMove.endTime.map(_ => leftMove))
  }
}
