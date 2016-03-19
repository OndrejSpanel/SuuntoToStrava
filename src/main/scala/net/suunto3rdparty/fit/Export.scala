package net.suunto3rdparty
package fit

import com.garmin.fit
import com.garmin.fit._
import java.io.File
import java.time.{Duration, ZonedDateTime}

import Util._
import net.suunto3rdparty.MoveHeader.ActivityType._

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

    val (sport, subsport) = move.header.moveType match {
      case RunningTrail => (Sport.RUNNING, SubSport.TRAIL)
      case RunningRoad => (Sport.RUNNING, SubSport.STREET)
      case Orienteering => (Sport.RUNNING, SubSport.TRAIL)
      case MountainBike => (Sport.CYCLING, SubSport.MOUNTAIN)
      case Cycling => (Sport.CYCLING, SubSport.ROAD)
      case Unknown => (Sport.GENERIC, SubSport.GENERIC)
    }
    val timeBeg = move.startTime.get
    val timeEnd = move.endTime.get

    // TODO: unify event sampling times (driven by best resolution)
    // we have the event stream sorted, now write it
    var openLap = false
    var lapCounter = 0
    var lastLapStart: ZonedDateTime = timeBeg
    def closeLap(time: ZonedDateTime): LapMesg = {

      val myMsg = new LapMesg()
      myMsg.setEvent(Event.LAP)
      myMsg.setEventType(EventType.STOP)
      myMsg.setStartTime(toTimestamp(lastLapStart))
      myMsg.setTimestamp(toTimestamp(time))
      myMsg.setMessageIndex(lapCounter)
      val lapDurationSec = Duration.between(lastLapStart, time).toMillis / 1000.0f
      lastLapStart = time
      lapCounter += 1
      myMsg.setTotalElapsedTime(lapDurationSec)
      myMsg.setTotalTimerTime(lapDurationSec)
      myMsg


    }
    for {
      evGroup <- combined
      ev <- evGroup._2
    } {
      val time = evGroup._1
      val msg: Option[Mesg] = ev match {
        case gps: GPSPoint =>
          val myMsg = new RecordMesg()
          val longLatScale = (1L << 31).toDouble / 180
          myMsg.setTimestamp(toTimestamp(time))
          myMsg.setPositionLong((gps.longitude * longLatScale).toInt)
          myMsg.setPositionLat((gps.latitude * longLatScale).toInt)
          Some(myMsg)
        case hr: HRPoint =>
          val myMsg = new RecordMesg()
          if (hr.hr != 0) {
            myMsg.setTimestamp(toTimestamp(time))
            myMsg.setHeartRate(hr.hr.toShort)
            myMsg.setDistance(hr.dist.toFloat)
            Some(myMsg)
          } else None
        case hr: Int =>
          val myMsg = new RecordMesg()
          myMsg.setTimestamp(toTimestamp(time))
          myMsg.setHeartRate(hr.toShort)
          Some(myMsg)
        case dist: Double =>
          val myMsg = new RecordMesg()
          myMsg.setTimestamp(toTimestamp(time))
          myMsg.setDistance(dist.toFloat)
          Some(myMsg)
        case lap: String =>
          if (timeDifference(time, timeEnd).abs < 5) {
            None
          } else if (openLap) {
            val lapMsg = closeLap(time)
            Some(lapMsg)
          } else {
            openLap = true
            None
          }
      }
      msg.foreach(encoder.onMesg)
    }

    if (openLap) {
      val lapMsg = closeLap(move.endTime.get)
      encoder.onMesg(lapMsg)
    }

    {
      val myMsg = new SessionMesg()
      myMsg.setStartTime(toTimestamp(timeBeg))
      myMsg.setTimestamp(toTimestamp(timeEnd))
      myMsg.setSport(sport)
      myMsg.setSubSport(subsport)
      val durationSec = timeDifference(timeBeg, timeEnd).toFloat
      myMsg.setTotalElapsedTime(durationSec)
      myMsg.setTotalTimerTime(durationSec)
      myMsg.setMessageIndex(0)
      myMsg.setFirstLapIndex(0)
      myMsg.setNumLaps(lapCounter + 1)

      myMsg.setEvent(Event.SESSION)
      myMsg.setEventType(EventType.STOP)

      encoder.onMesg(myMsg)
    }

    {
      val myMsg = new ActivityMesg()
      myMsg.setTimestamp(toTimestamp(timeEnd))
      myMsg.setNumSessions(1)
      myMsg.setType(Activity.MANUAL)
      myMsg.setEvent(Event.ACTIVITY)
      myMsg.setEventType(EventType.STOP)
      encoder.onMesg(myMsg)
    }

  }
  def toTimestamp(zonedTime: ZonedDateTime): DateTime = {
    val instant = zonedTime.toInstant
    val timestamp = instant.getEpochSecond - DateTime.OFFSET / 1000.0 + instant.getNano / 1000000000.0
    val dateTime = new DateTime(0, timestamp)
    dateTime
  }
}
