package net.suunto3rdparty

import java.time.ZonedDateTime

import scala.collection.immutable.SortedMap
import Util._

import scala.annotation.tailrec

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

  // drop beginning and end with no activity
  def dropAlmostEmpty: DataStream

  def toLog = s"$streamType: ${startTime.map(_.toLog)} .. ${endTime.map(_.toLogShort)}"
  override def toString = toLog

}

class DataStreamGPS(override val stream: SortedMap[ZonedDateTime, GPSPoint]) extends DataStream(StreamGPS) {
  type Item = GPSPoint

  override def pickData(data: DataMap) = new DataStreamGPS(data)

  private final case class GPSRect(latMin: Double, latMax: Double, lonMin: Double, lonMax: Double) {
    def this(item: GPSPoint) = {
      this(item.latitude, item.latitude, item.longitude, item.longitude)
    }

    def merge(that: GPSPoint) = {
      copy(
        latMin = that.latitude min latMin, latMax = that.latitude max latMax,
        lonMin = that.longitude min lonMin, lonMax = that.longitude max lonMax,
      )
    }

    // diagonal size of the rectangle
    def size: Double = {
      val R = 6371000 // Earth radius in metres
      val φ1 = latMin.toRadians
      val φ2 = latMax.toRadians
      val Δφ = φ2 - φ1
      val Δλ = (lonMax - lonMin).toRadians

      val a = Math.sin(Δφ / 2) * Math.sin(Δφ / 2) + Math.cos(φ1) * Math.cos(φ2) * Math.sin(Δλ / 2) * Math.sin(Δλ / 2)
      val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))

      val d = R * c // distance in meters
      d
    }
  }

  def rectAlmostEmpty(rect: GPSRect, timeBeg: ZonedDateTime, timeEnd: ZonedDateTime): Boolean = {
    val d = rect.size
    val duration = timeDifference(timeBeg, timeEnd).abs
    val maxSpeed = 0.05
    d < (maxSpeed * duration min 100)
  }

  override def isAlmostEmpty: Boolean = {
    val lat = stream.values.map(_.latitude)
    val lon = stream.values.map(_.longitude)
    // http://www.movable-type.co.uk/scripts/latlong.html
    val rect = GPSRect(lat.min, lat.max, lon.min, lon.max)

    rectAlmostEmpty(rect, stream.head._1, stream.last._1)
  }

  override def isNeeded = false
  // drop beginning and end with no activity
  private type ValueList = List[(ZonedDateTime, GPSPoint)]
  private def dropAlmostEmptyPrefix(stream: ValueList): ValueList = {

    @tailrec
    def dropAlmostEmptyPrefixFrom(begTime: ZonedDateTime, rect: GPSRect, stream: ValueList): ValueList = {
      stream match {
        case Nil => stream
        case head :: tail =>
          val newRect = rect merge head._2
          if (!rectAlmostEmpty(rect, begTime, head._1)) stream // TODO: return a bit
          else dropAlmostEmptyPrefixFrom(begTime, newRect, tail)
      }
    }

    dropAlmostEmptyPrefixFrom(stream.head._1, new GPSRect(stream.head._2), stream)
  }

  override def dropAlmostEmpty: DataStreamGPS = {
    val newStream = dropAlmostEmptyPrefix(dropAlmostEmptyPrefix(stream.toList).reverse).reverse
    new DataStreamGPS(SortedMap(newStream:_*))
  }
}

class DataStreamLap(override val stream: SortedMap[ZonedDateTime, String]) extends DataStream(StreamLap) {
  type Item = String

  override def pickData(data: DataMap) = new DataStreamLap(data)
  override def isAlmostEmpty = false
  override def isNeeded = true
  def dropAlmostEmpty: DataStreamLap = this
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
  def dropAlmostEmpty: DataStreamHRWithDist = this // TODO: drop

}

class DataStreamHR(override val stream: SortedMap[ZonedDateTime, Int]) extends DataStream(StreamHR) {
  type Item = Int

  override def pickData(data: DataMap) = new DataStreamHR(data)
  override def isAlmostEmpty = false
  override def isNeeded = false
  def dropAlmostEmpty: DataStreamHR = this // TODO: drop
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
  def dropAlmostEmpty: DataStreamDist = this // TODO: drop
}

