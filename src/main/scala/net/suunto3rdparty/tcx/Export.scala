package net.suunto3rdparty
package tcx

import java.io._
import java.time.ZonedDateTime

import MoveHeader.ActivityType._
import Util._
import resource._

import scala.collection.immutable.SortedMap

object Export {

  def toOutputStream(os: OutputStream, move: Move) {
    type MultiSamples = SortedMap[ZonedDateTime, Seq[DataStream#Item]]

    def toMultiSamples(data: DataStream#DataMap): MultiSamples = {
      data.mapValues(Seq(_))
    }

    def mergeMultiSamples(m: MultiSamples, d: DataStream#DataMap): MultiSamples = {
      val updateExisting = m.map { case (k, v) =>
        k -> (v ++ d.get(k))
      }
      val addNew = d.filterKeys(k => !m.contains(k)).mapValues(Seq(_))
      SortedMap(updateExisting.toSeq: _*) ++ addNew
    }

    // multiple items may share the same timestamp
    val combined = move.streams.tail.foldLeft(toMultiSamples(move.streams.head._2.stream)) { (comb, str) =>
      mergeMultiSamples(comb, str._2.stream)
    }

    val sport = move.header.moveType match {
      case RunningTrail => "Running"
      case RunningRoad => "Running"
      case Orienteering => "Running"
      case MountainBike => ""
      case Cycling => ""
      case Unknown => ""
    }
    val timeBeg = move.startTime.get
    val timeEnd = move.endTime.get

    val doc = <TrainingCenterDatabase xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2">
      <Activities>
        <Activity Sport={sport}>
        </Activity>

      </Activities>
    </TrainingCenterDatabase>

    for (writer <- managed(new OutputStreamWriter(os))) {
      scala.xml.XML.write(writer, doc, "UTF-8", xmlDecl = true, null)
    }
  }

  def apply(move: Move): Unit = {
    for (time <- move.streams.head._2.startTime) {
      val file = new File("testoutput_" + time.toFileName + ".tcx")

      for (fos <- managed(new FileOutputStream(file))) {
        toOutputStream(fos, move)
      }
    }

  }

}
