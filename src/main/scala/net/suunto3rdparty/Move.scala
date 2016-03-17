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

  val startTimeOpt = stream.headOption.map(_._1)
  val endTimeOpt = stream.lastOption.map(_._1)

  def startTime: ZonedDateTime = startTimeOpt.get
  def endTime: ZonedDateTime = endTimeOpt.get

  def isOverlapping(that: DataStream): Boolean = !(startTime > that.endTime || endTime < that.startTime)

  def takeUntil(time: ZonedDateTime): (DataStream, DataStream) = {
    val (take, left) = stream.span(_._1 < time)
    (pickData(take), pickData(left))
  }

  def toLog = s"$streamType: ${startTime.toLog} .. ${endTime.toLogShort}"

}

class DataStreamGPS(override val stream: SortedMap[ZonedDateTime, GPSPoint]) extends DataStream(StreamGPS) {
  type Item = GPSPoint

  override def pickData(data: DataMap) = new DataStreamGPS(data)
}

class DataStreamHRWithDist(override val stream: SortedMap[ZonedDateTime, HRPoint]) extends DataStream(StreamHRWithDist) {
  type Item = HRPoint

  override def pickData(data: DataMap) = new DataStreamHRWithDist(data)
}

class DataStreamHR(override val stream: SortedMap[ZonedDateTime, Int]) extends DataStream(StreamHR) {
  type Item = Int

  override def pickData(data: DataMap) = new DataStreamHR(data)
}

class DataStreamDist(override val stream: SortedMap[ZonedDateTime, Double]) extends DataStream(StreamDist) {
  type Item = Double

  override def pickData(data: DataMap) = new DataStreamDist(data)
}


object Move {
  implicit def ordering: Ordering[Move] = {
    new Ordering[Move] {
      override def compare(x: Move, y: Move) = x.startTime compareTo y.startTime
    }
  }
}

case class Move(header: MoveHeader, streams: Map[StreamType, DataStream]) {

  def this(header: MoveHeader, streamSeq: DataStream*) = {
    this(header, streamSeq.map(s => s.streamType -> s).toMap)
  }

  private def startTimeOfStreams(ss: Iterable[DataStream]) = ss.flatMap(_.startTimeOpt).min
  private def endTimeOfStreams(ss: Iterable[DataStream]) = ss.flatMap(_.endTimeOpt).max

  val startTime: ZonedDateTime = startTimeOfStreams(streams.values)
  val endTime: ZonedDateTime = endTimeOfStreams(streams.values)

  def toLog: String = streams.mapValues(_.toLog).mkString(", ")

  def addStream(stream: DataStream) = copy(streams = streams + (stream.streamType -> stream))

  def takeUntil(time: ZonedDateTime): (Move, Move) = {
    val split = streams.mapValues(_.takeUntil(time))

    val take = split.mapValues(_._1)
    val left = split.mapValues(_._2)

    (copy(streams = take), copy(streams = left))
  }
}
