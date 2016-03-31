package net.suunto3rdparty

import java.time.ZonedDateTime

import scala.collection.immutable.SortedMap
import Util._

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
object StreamLap extends StreamType {
  override def toString: String = "Lap"
}

object DataStream {
  def distanceIsAlmostEmpty(begDist: Double, endDist: Double, begTime: ZonedDateTime, endTime: ZonedDateTime): Boolean = {
    val dist = endDist - begDist
    val duration = timeDifference(begTime, endTime)
    val maxSpeed = 0.1
    dist < duration * maxSpeed

  }
}
sealed abstract class DataStream(val streamType: StreamType) {
  type Item

  type DataMap = SortedMap[ZonedDateTime, Item]

  def stream: DataMap

  def pickData(data: DataMap): DataStream

  val startTime: Option[ZonedDateTime] = stream.headOption.map(_._1)
  val endTime: Option[ZonedDateTime] = stream.lastOption.map(_._1)

  // should be discarded
  def isAlmostEmpty: Boolean

  // must not be discarded
  def isNeeded: Boolean

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

  override def isAlmostEmpty: Boolean = {
    val lat = stream.values.map(_.latitude)
    val lon = stream.values.map(_.longitude)
    // http://www.movable-type.co.uk/scripts/latlong.html

    val R = 6371000 // Earth radius in metres
    val φ1 = lat.min.toRadians
    val φ2 = lat.max.toRadians
    val Δφ = φ2 - φ1
    val Δλ = (lon.max - lon.min).toRadians

    val a = Math.sin(Δφ / 2) * Math.sin(Δφ / 2) + Math.cos(φ1) * Math.cos(φ2) * Math.sin(Δλ / 2) * Math.sin(Δλ / 2)
    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))

    val d = R * c // distance in meters
    val duration = timeDifference(stream.head._1, stream.last._1)
    val maxSpeed = 0.05
    d < (maxSpeed * duration min 100)
  } // TODO: implement (currently we decide based on the distance)

  override def isNeeded = false
}

class DataStreamLap(override val stream: SortedMap[ZonedDateTime, String]) extends DataStream(StreamLap) {
  type Item = String

  override def pickData(data: DataMap) = new DataStreamLap(data)
  override def isAlmostEmpty = false
  override def isNeeded = true
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

  override def isAlmostEmpty = DataStream.distanceIsAlmostEmpty(stream.head._2.dist, stream.last._2.dist, stream.head._1, stream.last._1)
  override def isNeeded = false

  override def pickData(data: DataMap) = new DataStreamHRWithDist(data).rebase

}

class DataStreamHR(override val stream: SortedMap[ZonedDateTime, Int]) extends DataStream(StreamHR) {
  type Item = Int

  override def pickData(data: DataMap) = new DataStreamHR(data)
  override def isAlmostEmpty = false
  override def isNeeded = false
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

  override def isAlmostEmpty = DataStream.distanceIsAlmostEmpty(stream.head._2, stream.last._2, stream.head._1, stream.last._1)
  override def isNeeded = false

  override def pickData(data: DataMap) = new DataStreamDist(data).rebase
}

