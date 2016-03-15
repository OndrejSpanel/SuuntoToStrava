package net.suunto3rdparty

import java.time.ZonedDateTime

import Util._

import scala.collection.immutable.SortedMap

case class Header(startTime: ZonedDateTime = ZonedDateTime.now, duration: Int = 0, calories: Int = 0, distance: Int = 0)
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

  val startTime: ZonedDateTime = stream.headOption.map(_._1).getOrElse(startTimeFromFile)
  def endTime: ZonedDateTime = startTime.plusNanos(durationMs * 1000000L)

  def isOverlapping(that: DataStream): Boolean = !(startTime > that.endTime || endTime < that.startTime)

  def toLog = s"$streamType: ${startTime.toLog} .. ${endTime.toLogShort}"

}

class DataStreamGPS(startTime: ZonedDateTime, durationMs: Int, override val stream: SortedMap[ZonedDateTime, GPSPoint]) extends DataStream(StreamGPS, startTime, durationMs) {
  type Item = GPSPoint
}

class DataStreamHRWithDist(startTime: ZonedDateTime, durationMs: Int, override val stream: SortedMap[ZonedDateTime, HRPoint]) extends DataStream(StreamHRWithDist, startTime, durationMs) {
  type Item = HRPoint
}

class DataStreamHR(startTime: ZonedDateTime, durationMs: Int, override val stream: SortedMap[ZonedDateTime, Int]) extends DataStream(StreamHR, startTime, durationMs) {
  type Item = Int
}

class DataStreamDist(startTime: ZonedDateTime, durationMs: Int, override val stream: SortedMap[ZonedDateTime, Double]) extends DataStream(StreamDist, startTime, durationMs) {
  type Item = Double
}

case class Move(header: Header, streams: Map[StreamType, DataStream]) {
  def this(header: Header, streamSeq: DataStream*) = {
    this(header, streamSeq.map(s => s.streamType -> s).toMap)
  }

  def toLog: String = streams.mapValues(_.toLog).mkString(", ")

  def addStream(stream: DataStream) = copy(streams = streams + (stream.streamType -> stream))
}
