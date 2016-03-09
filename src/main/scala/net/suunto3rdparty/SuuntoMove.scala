package net.suunto3rdparty

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.mutable

object SuuntoMove {private val dateFormat: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")}

class SuuntoMove {
  private var startTime: String = null
  private var duration: Int = 0
  private var calories: Int = 0
  private var distance: Int = 0
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
  def getStartTime: String = {
    startTime
  }
  def setStartTime(startTime: String): Unit =  {
    this.startTime = startTime
  }
  def setStartTime(startTime: Date): Unit =  {
    this.startTime = SuuntoMove.dateFormat.format(startTime)
  }
  def getDuration: Int = duration
  def setDuration(duration: Int): Unit =  {
    this.duration = duration
  }
  def getCalories: Int = calories
  def setCalories(calories: Int): Unit =  {
    this.calories = calories
  }
  def getDistance: Int = distance
  def setDistance(distance: Int): Unit = {
    this.distance = distance
  }
  def addTrackPoint(lat: Double, lon: Double, ele: Int, time: String): Unit = {
    trackPoints += new TrackPoint(lat, lon, ele, time)
  }
  def getTrackPoints: Seq[TrackPoint] = trackPoints

  override def equals(obj: Any): Boolean = {
    val move: SuuntoMove = obj.asInstanceOf[SuuntoMove]
    if ((move.getDistance == this.getDistance) && (move.getCalories == this.getCalories) && (move.getDuration == this.getDuration) && (move.getStartTime == this.getStartTime)) {
      return true
    }
    false
  }

  class TrackPoint(var latitude: Double, var longitude: Double, var elevation: Int, var time: String) {
    def getLatitude: String = latitude.toString
    def getLongitude: String = longitude.toString
    def getElevation: Int = elevation
    def getTime: String = time
  }

}