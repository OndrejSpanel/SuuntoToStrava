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

  def timeOffset(bestOffset: Int): DataStream = {
    val adjusted = stream.map{
      case (k,v) =>
        k.plus(Duration.ofSeconds(bestOffset)) -> v
    }
    pickData(adjusted)
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

  /**
    * Experiments have shown smoothingInterval = 60 gives most accurate results.
    * Perhaps the same smoothing interval is used in the Quest itself?
    */
  private val smoothingInterval = 60

  /**
    * Quest records sometimes miss one sample, the missing sample is added the a neighboring sample, like:
    * 2016-04-13T09:47:01Z	4.9468178241
    * 2016-04-13T09:47:02Z	10.5000627924
    * 2016-04-13T09:47:04Z	5.2888359044
    */
  private def fixSpeed(input: DistStream): DistStream = {
    def fixSpeedRecurse(input: DistStream, done: DistStream): DistStream = {
      if (input.isEmpty) done
      else {
        if (input.tail.isEmpty) fixSpeedRecurse(input.tail, done + input.head)
        else {
          val item0 = input.head
          val item1 = input.tail.head
          val duration = Duration.between(input.head._1, input.tail.head._1).getSeconds
          if (duration>1) {
            // missing sample
            val missingCount = duration - 1
            val value0 = item0._2
            val value1 = item1._2
            if (value0 >= value1) {
              // 0 large, 1 missing, 2 small
              val fixed0 = item0.copy(_2 = value0 / duration)
              val addMissing = for (s <- 1 to missingCount.toInt) yield fixed0.copy(_1 = item0._1.plusSeconds(s))
              fixSpeedRecurse(input.tail, done + fixed0 ++ addMissing)
            } else {
              // 0 small, 1 missing, 2 large
              val fixed2 = item1.copy(_2 = value0 / duration)
              val addMissing = for (s <- 1 to missingCount.toInt) yield fixed2.copy(_1 = item0._1.plusSeconds(s))
              fixSpeedRecurse(input.tail.tail, done + item0 ++ addMissing + fixed2)
            }
          } else {
            fixSpeedRecurse(input.tail, done + input.head)
          }

        }
      }
    }

    fixSpeedRecurse(input, SortedMap())
  }

  private def smoothSpeed(input: DistStream, durationSec: Double): DistStream = {
    def smoothingRecurse(done: DistStream, prev: DistStream, todo: DistStream): DistStream = {
      if (todo.isEmpty) done
      else if (prev.isEmpty) {
        smoothingRecurse(done + todo.head, prev + todo.head, todo.tail)
      } else {
        def durationWindow(win: DistStream) = Duration.between(win.keys.head, win.keys.last).getSeconds
        def keepWindow(win: DistStream): DistStream = if (durationWindow(win) <= durationSec) win else keepWindow(win.tail)
        val newWindow = keepWindow(prev + todo.head)
        val duration = durationWindow(newWindow)
        val windowSpeed = if (duration > 0) prev.values.sum / duration else 0.0
        val interval = Duration.between(prev.last._1, todo.head._1).getSeconds
        val smoothDist = (windowSpeed * duration + todo.head._2) / ( duration + interval)
        smoothingRecurse(done + (todo.head._1 -> smoothDist), newWindow, todo.tail)
      }
    }

    smoothingRecurse(SortedMap(), SortedMap(), fixSpeed(input))
  }

  private def distStreamFromGPS(gps: SortedMap[ZonedDateTime, GPSPoint]) = {
    val gpsPairs = SortedMap((gps.keys zip (gps.values zip gps.values.tail)).toSeq: _*)
    val gpsDistances = gpsPairs.mapValues { case (a, b) =>
      val rect = new GPSRect(a).merge(b)
      rect.size
    }
    gpsDistances
  }

  private def computeSpeedStream: DistStream = {

    val gpsDistances = distStreamFromGPS(stream)

    val smoothedSpeed = smoothSpeed(gpsDistances, smoothingInterval)
    smoothedSpeed
  }


  private def distStreamToCSV(ds: DistStream): String = {
    ds.map(kv => s"${kv._1},${kv._2}").mkString("\n")
  }

  private def rawToCSV: String = {
    val dist = distStreamFromGPS(stream)
    distStreamToCSV(dist)
  }

  private def smoothedToCSV: String = {
    val dist = distStreamFromGPS(stream)
    val smooth = smoothSpeed(dist, smoothingInterval)
    distStreamToCSV(smooth)
  }

  /*
  * @param timeOffset in seconds
  * */
  private def errorToStream(offsetStream: DistStream, speedStream: DistStream): Double = {
    if (offsetStream.isEmpty || speedStream.isEmpty) {
      Double.MaxValue
    } else {
      // TODO: optimize: move speed smoothing out of this function
      def maxTime(a: ZonedDateTime, b: ZonedDateTime) = if (a>b) a else b
      def minTime(a: ZonedDateTime, b: ZonedDateTime) = if (a<b) a else b
      val begMatch = maxTime(offsetStream.head._1, startTime.get)
      val endMatch = minTime(offsetStream.last._1, endTime.get)
      // ignore non-matching parts (prefix, postfix)
      def selectInner[T](data: SortedMap[ZonedDateTime, T]) = data.dropWhile(_._1 < begMatch).takeWhile(_._1 < endMatch)
      val distToMatch = selectInner(offsetStream)

      val speedToMatch = (distToMatch zip distToMatch.tail).map {
        case ((aTime, aDist), (bTime, bDist)) => aTime -> aDist / Duration.between(aTime, bTime).getSeconds
      }
      val smoothedSpeed = selectInner(speedStream)

      def compareSpeedHistory(fineSpeed: DistStream, coarseSpeed: DistStream, error: Double): Double = {
        //
        if (fineSpeed.isEmpty || coarseSpeed.isEmpty) error
        else {
          if (fineSpeed.head._1 < coarseSpeed.head._1) compareSpeedHistory(fineSpeed.tail, coarseSpeed, error)
          else {
            def square(x: Double) = x * x
            val itemError = square(fineSpeed.head._2 - coarseSpeed.head._2)
            compareSpeedHistory(fineSpeed.tail, coarseSpeed.tail, error + itemError * itemError)
          }
        }
      }

      val error = compareSpeedHistory(smoothedSpeed, speedToMatch, 0)

      error
    }

  }

  /*
  * @param 10 sec distance stream (provided by a Quest) */
  private def findOffset(distanceStream: DistStream) = {
    val maxOffset = 60
    val offsets = -maxOffset to maxOffset
    val speedStream = computeSpeedStream
    val errors = for (offset <- offsets) yield {
      val offsetStream = distanceStream.map { case (k,v) =>
        k.plus(Duration.ofSeconds(offset)) -> v
      }
      errorToStream(offsetStream, speedStream)
    }
    // TODO: prefer most central best error
    val (minError, minErrorOffset) = (errors zip offsets).minBy(_._1)
    // compute confidence: how much is the one we have selected reliable?
    // the ones close may have similar metrics, that is expected, but none far away should have it


    def confidenceForSolution(offsetCandidate: Int) = {
      val confidences = (errors zip offsets).map { case (err, off) =>
        if (off == offsetCandidate) 0
        else {
          val close = 1 - (off - offsetCandidate).abs / (2 * maxOffset).toDouble
          (err - minError) * close
        }
      }

      val confidence = confidences.sum
      confidence
    }

    (minErrorOffset, confidenceForSolution(minErrorOffset))
  }

  def adjustHrd(hrdMove: Move): Move = {
    val hrWithDistStream = hrdMove.streams.get(StreamHRWithDist)
    hrWithDistStream.map { dist =>
      val distTyped = dist.asInstanceOf[DataStreamHRWithDist]
      val distanceSums = distTyped.stream.mapValues(_.dist)
      val distances = (distTyped.stream.values.tail zip distTyped.stream.values).map(ab => ab._1.dist - ab._2.dist)
      //val distances10x = distances.flatMap(d => List.fill(10)(d/10)).mkString("\n")
      val distancesWithTimes = SortedMap((distanceSums.keys zip distances).toSeq:_*)
      val (bestOffset, confidence) = findOffset(distancesWithTimes)
      println(s"Quest offset $bestOffset from distance ${distanceSums.last._2}, confidence $confidence")
      //hrdMove.timeOffset(bestOffset)
      hrdMove
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

