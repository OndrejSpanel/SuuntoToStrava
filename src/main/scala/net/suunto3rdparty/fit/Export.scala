package net.suunto3rdparty
package fit

import com.garmin.fit
import com.garmin.fit._
import java.io.{File, OutputStream}
import java.time.ZonedDateTime

import Util._

import scala.collection.immutable.SortedMap

object Export {
  type Encoder = MesgListener with MesgDefinitionListener

  private def createFileEncoder(time: ZonedDateTime): FileEncoder = {
    val file = new File("testoutput_" + time.toFileName + ".fit")
    new FileEncoder(file)
  }

  object suuntoToFit {
    def activityType(act: Int): (ActivityType, ActivitySubtype) = act match {
      case 82 => (ActivityType.RUNNING, ActivitySubtype.TRACK)
      case 5 => (ActivityType.CYCLING, ActivitySubtype.TRACK_CYCLING)
      case _ => (ActivityType.GENERIC, ActivitySubtype.GENERIC)
    }
  }

  def encodeHeader(encoder: Encoder, header: MoveHeader): Unit = {
    //Generate FileIdMessage
    val fileIdMesg = new FileIdMesg
    fileIdMesg.setManufacturer(Manufacturer.SUUNTO)
    fileIdMesg.setType(fit.File.ACTIVITY)
    fileIdMesg.setProduct(1000) // TODO: use from Move of find a value
    fileIdMesg.setSerialNumber(12345L) // TODO: use from Move of find a value

    encoder.onMesg(fileIdMesg)
  }

  def apply(move: Move): Unit = {
    for (time <- move.streams.head._2.startTime) {
      val encoder = createFileEncoder(time)
      toEncoder(move, encoder)

      encoder.close()
    }

  }

  def toBuffer(move: Move): Array[Byte] = {
    val encoder = new BufferEncoder()

    toEncoder(move, encoder)

    encoder.close()
  }

  def toEncoder(move: Move, encoder: Encoder): Unit = {

    // start by writing a header
    encodeHeader(encoder, move.header)

    // write all data, sorted by time

    type MultiSamples = SortedMap[ZonedDateTime, Seq[DataStream#Item]]

    def toMultiSamples(data: DataStream#DataMap): MultiSamples = {
      data.mapValues(Seq(_))
    }

    def mergeMultiSamples(m: MultiSamples, d: DataStream#DataMap): MultiSamples = {
      val updateExisting = m.map { case (k, v) =>
        k -> (v ++ d.get(k))
      }
      val addNew = d.filterKeys(k => !m.contains(k)).mapValues(Seq(_))
      SortedMap(updateExisting.toSeq:_*) ++ addNew
    }

    // multiple items may share the same timestamp
    val combined = move.streams.tail.foldLeft(toMultiSamples(move.streams.head._2.stream)) { (comb, str) =>
      mergeMultiSamples(comb, str._2.stream)
    }

    // TODO: unify event sampling times (driven by best resolution)
    // we have the event stream sorted, now write it
    for {
      evGroup <- combined
      ev <- evGroup._2
    } {
      val msg: Option[Mesg] = ev match {
        case gps: GPSPoint =>
          val myMsg = new RecordMesg()
          val longLatScale = (1L<<31).toDouble/180
          myMsg.setTimestamp(toTimestamp(evGroup._1))
          myMsg.setPositionLong((gps.longitude * longLatScale).toInt)
          myMsg.setPositionLat((gps.latitude * longLatScale).toInt)
          Some(myMsg)
        case hr: HRPoint =>
          val myMsg = new RecordMesg()
          if (hr.hr!=0) {
            myMsg.setTimestamp(toTimestamp(evGroup._1))
            myMsg.setHeartRate(hr.hr.toShort)
            myMsg.setDistance(hr.dist.toFloat)
            Some(myMsg)
          } else None
        case hr: Int =>
          val myMsg = new RecordMesg()
          myMsg.setTimestamp(toTimestamp(evGroup._1))
          myMsg.setHeartRate(hr.toShort)
          Some(myMsg)
        case dist: Double =>
          val myMsg = new RecordMesg()
          myMsg.setTimestamp(toTimestamp(evGroup._1))
          myMsg.setDistance(dist.toFloat)
          Some(myMsg)
      }
      msg.foreach(encoder.onMesg)
    }

  }
  def toTimestamp(zonedTime: ZonedDateTime): DateTime = {
    val instant = zonedTime.toInstant
    val timestamp = instant.getEpochSecond - DateTime.OFFSET / 1000.0 + instant.getNano / 1000000000.0
    val dateTime = new DateTime(0, timestamp)
    dateTime
  }
}
