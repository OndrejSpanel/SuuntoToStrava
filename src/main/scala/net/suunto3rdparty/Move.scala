package net.suunto3rdparty

import java.time.{Duration, ZonedDateTime}

import Util._

import scala.reflect._

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

  def convertMap(streamSeq: Seq[DataStream]): Map[Class[_], DataStream] = {
    streamSeq.map(s => s.streamType -> s).toMap
  }
}

case class Move(fileName: Set[String], header: MoveHeader, streams: Map[Class[_], DataStream]) {
  def timeOffset(bestOffset: Int): Move = {
    copy(streams = streams.mapValues(_.timeOffset(bestOffset)))
  }

  def this(fileName: Set[String], header: MoveHeader, streamSeq: DataStream*) = {
    this(fileName, header, Move.convertMap(streamSeq))
  }

  def stream[T <: DataStream: ClassTag]: T = streams(classTag[T].runtimeClass).asInstanceOf[T]

  def streamGet[T <: DataStream: ClassTag]: Option[T] = streams.get(classTag[T].runtimeClass).map(_.asInstanceOf[T])

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
  def isAlmostEmpty(minDurationSec: Long) = {
    !streams.exists(_._2.stream.nonEmpty) || endTime.get < startTime.get.plusSeconds(minDurationSec) || streams.exists(x => x._2.isAlmostEmpty)
  }

  def toLog: String = streams.values.map(_.toLog).mkString(", ")

  def addStream(streamSource: Move, stream: DataStream): Move = {
    copy(streams = streams + (stream.streamType -> stream), fileName = fileName ++ streamSource.fileName, header = header.merge(streamSource.header))
  }

  def span(time: ZonedDateTime): (Option[Move], Option[Move]) = {
    val split = streams.mapValues(_.span(time))

    val take = split.mapValues(_._1)
    val left = split.mapValues(_._2)

    val takeMove = copy(streams = take)
    val leftMove = copy(streams = left)
    (takeMove.endTime.map(_ => takeMove), leftMove.endTime.map(_ => leftMove))
  }
}
