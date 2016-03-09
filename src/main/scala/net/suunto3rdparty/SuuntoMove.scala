package net.suunto3rdparty

import java.text.SimpleDateFormat

import scala.collection.mutable

object SuuntoMove {val dateFormat: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")}

case class SuuntoMove(var startTime: String = "", var duration: Int = 0, var calories: Int = 0, var distance: Int = 0) {
  def this(distSamples: Seq[Int], hrSamples: Seq[Int]) = {
    this()
    distanceSamples ++= distSamples
    heartRateSamples ++= hrSamples
  }

  private var distanceSamples = mutable.ArrayBuffer[Int]()
  private var heartRateSamples = mutable.ArrayBuffer[Int]()
  private var trackPoints = mutable.ArrayBuffer[TrackPoint]()

  def addDistanceSample(distance: Int): Unit =  {
    distanceSamples += distance
  }
  def addHeartRateSample(heartRate: Int): Unit =  {
    heartRateSamples += heartRate
  }
  def addTrackPoint(lat: Double, lon: Double, ele: Int, time: String): Unit = {
    trackPoints += TrackPoint(lat, lon, ele, time)
  }

  case class TrackPoint(latitude: Double, longitude: Double, elevation: Int, time: String)

}