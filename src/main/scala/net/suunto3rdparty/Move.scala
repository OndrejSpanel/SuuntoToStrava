package net.suunto3rdparty

import java.time.ZonedDateTime

import Util._

import scala.collection.immutable.SortedMap

case class Header(startTime: ZonedDateTime = ZonedDateTime.now, durationMs: Int = 0, calories: Int = 0, distance: Int = 0)
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

sealed abstract class DataStream(val streamType: StreamType, val startTimeFromFile: ZonedDateTime, val durationMs: Int) {

  type Item

  type DataMap = SortedMap[ZonedDateTime, Item]

  def stream: DataMap

  def pickData(data: DataMap): DataStream

  val startTime: ZonedDateTime = stream.headOption.map(_._1).getOrElse(startTimeFromFile)
  def endTime: ZonedDateTime = stream.lastOption.map(_._1).getOrElse(startTimeFromFile)

  def isOverlapping(that: DataStream): Boolean = !(startTime > that.endTime || endTime < that.startTime)

  def takeUntil(time: ZonedDateTime): (DataStream, DataStream) = {
    val (take, left) = stream.span(_._1 < time)
    (pickData(take), pickData(left))
  }

  def toLog = s"$streamType: ${startTime.toLog} .. ${endTime.toLogShort}"

}

class DataStreamGPS(startTime: ZonedDateTime, durationMs: Int, override val stream: SortedMap[ZonedDateTime, GPSPoint]) extends DataStream(StreamGPS, startTime, durationMs) {
  type Item = GPSPoint

  override def pickData(data: DataMap) = new DataStreamGPS(startTime, durationMs, data)
}

class DataStreamHRWithDist(startTime: ZonedDateTime, durationMs: Int, override val stream: SortedMap[ZonedDateTime, HRPoint]) extends DataStream(StreamHRWithDist, startTime, durationMs) {
  type Item = HRPoint

  override def pickData(data: DataMap) = new DataStreamHRWithDist(startTime, durationMs, data)
}

class DataStreamHR(startTime: ZonedDateTime, durationMs: Int, override val stream: SortedMap[ZonedDateTime, Int]) extends DataStream(StreamHR, startTime, durationMs) {
  type Item = Int

  override def pickData(data: DataMap) = new DataStreamHR(startTime, durationMs, data)
}

class DataStreamDist(startTime: ZonedDateTime, durationMs: Int, override val stream: SortedMap[ZonedDateTime, Double]) extends DataStream(StreamDist, startTime, durationMs) {
  type Item = Double

  override def pickData(data: DataMap) = new DataStreamDist(startTime, durationMs, data)
}

case class Move(header: Header, streams: Map[StreamType, DataStream]) {

  def this(header: Header, streamSeq: DataStream*) = {
    this(header, streamSeq.map(s => s.streamType -> s).toMap)
  }

  def startTime: ZonedDateTime = streams.values.map(_.startTime).min
  def endTime: ZonedDateTime = streams.values.map(_.endTime).max

  def toLog: String = streams.mapValues(_.toLog).mkString(", ")

  def addStream(stream: DataStream) = copy(streams = streams + (stream.streamType -> stream))

  def takeUntil(time: ZonedDateTime): (Move, Move) = {
    val split = streams.mapValues(_.takeUntil(time))

    val take = split.mapValues(_._1)
    val left = split.mapValues(_._2)

    (copy(streams = take), copy(streams = left))
  }
}
