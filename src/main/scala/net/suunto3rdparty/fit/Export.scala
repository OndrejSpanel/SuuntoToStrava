package net.suunto3rdparty
package fit

import com.garmin.fit
import com.garmin.fit._
import java.io.File
import java.time.ZonedDateTime

import Util._

import scala.collection.immutable.SortedMap

trait EncoderCloser {
  def close()
}

object Export {
  type Encoder = MesgListener with MesgDefinitionListener with EncoderCloser

  def createEncoder(time: ZonedDateTime): Encoder = {
    val file = new File("testoutput_" + time.toFileName + ".fit")
    val encoder = new FileEncoder(file) with EncoderCloser

    //Generate FileIdMessage
    val fileIdMesg = new FileIdMesg
    fileIdMesg.setManufacturer(Manufacturer.SUUNTO)
    fileIdMesg.setType(fit.File.ACTIVITY)
    fileIdMesg.setProduct(1000) // TODO: use from Move of find a value
    fileIdMesg.setSerialNumber(12345L) // TODO: use from Move of find a value

    encoder.write(fileIdMesg)

    encoder
  }


  def apply(move: Move): Unit = {

    val encoder = createEncoder(move.streams.head._2.startTime)
    // start by writing a header
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
          val myMsg = new CoursePointMesg()
          val longLatScale = 1000000
          val instantMs = evGroup._1.toInstant.getEpochSecond * 1000 + evGroup._1.toInstant.getNano / 1000000
          myMsg.setTimestamp(new DateTime(instantMs))
          myMsg.setPositionLong((gps.longitude*longLatScale).toInt)
          myMsg.setPositionLat((gps.latitude*longLatScale).toInt)
          Some(myMsg)
        case hr: Int =>
          None
        case dist: Double =>
          None
      }
      msg.foreach(encoder.onMesg)
    }

    // file needs closing
    encoder.close()

  }
}
