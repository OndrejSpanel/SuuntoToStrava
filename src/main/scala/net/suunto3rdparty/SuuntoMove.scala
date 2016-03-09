package net.suunto3rdparty

import java.text.SimpleDateFormat

import scala.collection.mutable

object SuuntoMove {val dateFormat: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")}

case class SuuntoMove(var startTime: String = "", var duration: Int = 0, var calories: Int = 0, var distance: Int = 0) {
  private var distanceSamples = mutable.ArrayBuffer[Int]()
  private var heartRateSamples = mutable.ArrayBuffer[Int]()
  private var trackPoints = mutable.ArrayBuffer[TrackPoint]()

  def getDistanceSamples: Seq[Int] = distanceSamples
  def getHeartRateSamples: Seq[Int] = heartRateSamples

  def addDistanceSample(distance: Int): Unit =  {
    distanceSamples += distance
  }
  def addHeartRateSample(heartRate: Int): Unit =  {
    heartRateSamples += heartRate
  }
  def addTrackPoint(lat: Double, lon: Double, ele: Int, time: String): Unit = {
    trackPoints += new TrackPoint(lat, lon, ele, time)
  }
  def getTrackPoints: Seq[TrackPoint] = trackPoints

  case class TrackPoint(latitude: Double, longitude: Double, elevation: Int, time: String) {
    def getLatitude: String = latitude.toString
    def getLongitude: String = longitude.toString
    def getElevation: Int = elevation
    def getTime: String = time
  }

}