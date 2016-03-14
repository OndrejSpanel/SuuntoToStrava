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

  def createEncoder: Encoder = {
    val file = new File("testoutput.fit")
    val encoder = new FileEncoder(file) with EncoderCloser

    //Generate FileIdMessage
    val fileIdMesg = new FileIdMesg
    fileIdMesg.setManufacturer(Manufacturer.DYNASTREAM)
    fileIdMesg.setType(fit.File.SETTINGS)
    fileIdMesg.setProduct(1000)
    fileIdMesg.setSerialNumber(12345L)

    encoder.write(fileIdMesg)

    encoder
  }


  def apply(move: Move): Unit = {

    val encoder = createEncoder
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

    // file needs closing
    encoder.close()

  }
}
