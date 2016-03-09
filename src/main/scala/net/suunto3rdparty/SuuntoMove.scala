package net.suunto3rdparty

import java.text.SimpleDateFormat
import java.util.Date

object SuuntoMove {private val dateFormat: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")}

class SuuntoMove {
  distanceSamples = new util.ArrayList[Integer]
  heartRateSamples = new util.ArrayList[Integer]
  trackPoints = new util.ArrayList[SuuntoMove#TrackPoint]
  private var startTime: String = null
  private var duration: Int = 0
  private var calories: Int = 0
  private var distance: Int = 0
  private var distanceSamples: util.ArrayList[Integer] = null
  private var heartRateSamples: util.ArrayList[Integer] = null
  private var trackPoints: util.ArrayList[SuuntoMove#TrackPoint] = null
  def getDistanceSamples: util.ArrayList[Integer] = {
    return distanceSamples
  }
  def getHeartRateSamples: util.ArrayList[Integer] = {
    return heartRateSamples
  }
  def addDistanceSample(distance: Integer) {
    distanceSamples.add(distance)
  }
  def addHeartRateSample(heartRate: Integer) {
    heartRateSamples.add(heartRate)
  }
  def getStartTime: String = {
    return startTime
  }
  def setStartTime(startTime: String) {
    this.startTime = startTime
  }
  def setStartTime(startTime: Date) {
    this.startTime = SuuntoMove.dateFormat.format(startTime)
  }
  def getDuration: Int = {
    return duration
  }
  def setDuration(duration: Int) {
    this.duration = duration
  }
  def getCalories: Int = {
    return calories
  }
  def setCalories(calories: Int) {
    this.calories = calories
  }
  def getDistance: Int = {
    return distance
  }
  def setDistance(distance: Int) {
    this.distance = distance
  }
  def addTrackPoint(lat: Double, lon: Double, ele: Int, time: String) {
    trackPoints.add(new SuuntoMove#TrackPoint(lat, lon, ele, time))
  }
  def getTrackPoints: Iterable[SuuntoMove#TrackPoint] = {
    return trackPoints
  }
  override def equals(obj: Any): Boolean = {
    val move: SuuntoMove = obj.asInstanceOf[SuuntoMove]
    if ((move.getDistance == this.getDistance) && (move.getCalories == this.getCalories) && (move.getDuration == this.getDuration) && (move.getStartTime == this.getStartTime)) {
      return true
    }
    return false
  }

  class TrackPoint(var latitude: Double, var longitude: Double, var elevation: Int, var time: String) {
    def getLatitude: String = {
      return Double.toString(latitude)
    }
    def getLongitude: String = {
      return Double.toString(longitude)
    }
    def getElevation: Int = {
      return elevation
    }
    def getTime: String = {
      return time
    }
  }

}