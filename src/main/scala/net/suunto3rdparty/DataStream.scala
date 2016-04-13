package net.suunto3rdparty

import java.time.{Duration, ZonedDateTime}

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

  def toLog = s"$streamType: ${startTime.map(_.toLog).getOrElse("")} .. ${endTime.map(_.toLogShort).getOrElse("")}"

  override def toString = toLog

}

object DataStreamGPS {
  private final case class GPSRect(latMin: Double, latMax: Double, lonMin: Double, lonMax: Double) {
    def this(item: GPSPoint) = {
      this(item.latitude, item.latitude, item.longitude, item.longitude)
    }

    def merge(that: GPSPoint) = {
      copy(
        latMin = that.latitude min latMin, latMax = that.latitude max latMax,
        lonMin = that.longitude min lonMin, lonMax = that.longitude max lonMax
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
    val maxSpeed = 0.2
    d <= (maxSpeed * duration min 100)
  }

}

class DataStreamGPS(override val stream: SortedMap[ZonedDateTime, GPSPoint]) extends DataStream(StreamGPS) {

  import DataStreamGPS._

  type Item = GPSPoint

  override def pickData(data: DataMap) = new DataStreamGPS(data)

  override def isAlmostEmpty: Boolean = {
    if (stream.isEmpty) true
    else {
      val lat = stream.values.map(_.latitude)
      val lon = stream.values.map(_.longitude)
      // http://www.movable-type.co.uk/scripts/latlong.html
      val rect = GPSRect(lat.min, lat.max, lon.min, lon.max)

      rectAlmostEmpty(rect, stream.head._1, stream.last._1)
    }
  }

  override def isNeeded = false
  // drop beginning and end with no activity
  private type ValueList = List[(ZonedDateTime, GPSPoint)]

  override def dropAlmostEmpty: DataStreamGPS = {
    if (stream.nonEmpty) {
      @tailrec
      def detectEmptyPrefix(begTime: ZonedDateTime, rect: GPSRect, stream: ValueList, ret: Option[ZonedDateTime]): Option[ZonedDateTime] = {
        stream match {
          case Nil => ret
          case head :: tail =>
            val newRect = rect merge head._2
            val newRet = if (rectAlmostEmpty(rect, begTime, head._1)) Some(head._1) else ret
            detectEmptyPrefix(begTime, newRect, tail, newRet)
        }
      }

      def dropEmptyPrefix(stream: ValueList, timeOffset: Duration, compare: (ZonedDateTime, ZonedDateTime) => Boolean) = {
        val prefixTime = detectEmptyPrefix(stream.head._1, new GPSRect(stream.head._2), stream, None)
        prefixTime.map { prefTime =>
          val offsetPrefTime = prefTime.plus(timeOffset)
          stream.dropWhile(t => compare(t._1, offsetPrefTime))
        }.getOrElse(stream)
      }

      val droppedPrefix = dropEmptyPrefix(stream.toList, Duration.ofSeconds(-10), _ <= _)
      val droppedPostfix = dropEmptyPrefix(droppedPrefix.reverse, Duration.ofSeconds(+10), _ >= _)
      new DataStreamGPS(SortedMap(droppedPostfix: _*))
    } else this
  }

  private type DistStream  = SortedMap[ZonedDateTime, Double]

  /*
  * @param timeOffset in seconds
  * */
  private def errorToStream(offsetStream: DistStream): Double = {
    // ignore non-matching parts (prefix, postfix)
    if (offsetStream.isEmpty || stream.isEmpty) {
      0
    } else {
      def maxTime(a: ZonedDateTime, b: ZonedDateTime) = if (a>b) a else b
      def minTime(a: ZonedDateTime, b: ZonedDateTime) = if (a<b) a else b
      val begMatch = maxTime(offsetStream.head._1, startTime.get)
      val endMatch = minTime(offsetStream.last._1, endTime.get)
      val distToMatch = offsetStream.dropWhile(_._1 < begMatch).takeWhile(_._1 < endMatch)
      val gpsToMatch = stream.dropWhile(_._1 < begMatch).takeWhile(_._1 < endMatch)

      // assume d1 is finer
      def processDistances(gps: SortedMap[ZonedDateTime, GPSPoint], d2: DistStream, ret: DistStream, lastGPS: GPSPoint): DistStream = {
        if (gps.isEmpty || d2.isEmpty) ret
        else {
          if (gps.head._1 < d2.head._1) {
            processDistances(gps.tail, d2, ret, lastGPS)
          } else {
            val gpsRect = new GPSRect(lastGPS).merge(gps.head._2)
            processDistances(gps.tail, d2.tail, ret + (gps.head._1 -> gpsRect.size), gps.head._2)
          }
        }
      }

      // assume gpsDistances is finer
      val gpsDistStream = processDistances(gpsToMatch, distToMatch, SortedMap(), gpsToMatch.head._2)

      val differences = (gpsDistStream.values zip distToMatch.values).map(ab => ab._1 - ab._2)
      val error = differences.map(x => x*x).sum

      error
    }

  }

  /*
  * @param 10 sec distance stream (provided by a Quest) */
  private def findOffset(distanceStream: DistStream) = {
    val maxOffset = 60
    val offsets = -maxOffset to maxOffset
    val errors = for (offset <- offsets) yield {
      val offsetStream = distanceStream.map { case (k,v) =>
        k.plus(Duration.ofSeconds(offset)) -> v
      }
      errorToStream(offsetStream)
    }
    val minErrorIndex = errors.zipWithIndex.minBy(_._1)._2
    // TODO: prefer most central best error
    // if 0 has the same error as the best error, prefer it
    val prefer0 = if (errors(maxOffset) == errors(minErrorIndex)) offsets(maxOffset)
    else offsets(minErrorIndex)
    prefer0
  }

  def adjustHrd(hrdMove: Move): Move = {
    val hrWithDistStream = hrdMove.streams.get(StreamHRWithDist)
    hrWithDistStream.map { dist =>
      val distTyped = dist.asInstanceOf[DataStreamHRWithDist]
      val distanceSums = distTyped.stream.mapValues(_.dist)
      val distances = (distTyped.stream.values.tail zip distTyped.stream.values).map(ab => ab._1.dist - ab._2.dist)
      val distancesWithTimes = SortedMap((distanceSums.keys zip distances).toSeq:_*)
      val bestOffset = findOffset(distancesWithTimes)
      val adjusted = distTyped.stream.map { case (k,v) =>
        k.plus(Duration.ofSeconds(bestOffset)) -> v
      }
      hrdMove.addStream(hrdMove, new DataStreamHRWithDist(adjusted))
    }.getOrElse(hrdMove)
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

