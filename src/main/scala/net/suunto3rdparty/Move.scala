package net.suunto3rdparty

import java.time.{Duration, ZonedDateTime}

import Util._

case class Header(moveHeader: MoveHeader, startTime: ZonedDateTime = ZonedDateTime.now, durationMs: Int = 0, calories: Int = 0, distance: Int = 0)

case class Lap(name: String, timestamp: ZonedDateTime)

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

  def duration: Double = {
    val durOpt = for (beg <- startTime; end <- endTime) yield {
      Duration.between(beg, end).toMillis / 1000.0
    }
    durOpt.getOrElse(0.0)
  }


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
