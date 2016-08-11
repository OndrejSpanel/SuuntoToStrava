package net.suunto3rdparty

object GPS {
  // distance of the GPS points (lat/lon in degrees)
  def distance(latMin: Double, lonMin: Double, latMax: Double, lonMax: Double): Double = {
    val R = 6371000 // Earth radius in metres
    val φ1 = latMin.toRadians
    val φ2 = latMax.toRadians
    val Δφ = φ2 - φ1
    val Δλ = (lonMax - lonMin).toRadians

    val a = Math.sin(Δφ / 2) * Math.sin(Δφ / 2) + Math.cos(φ1) * Math.cos(φ2) * Math.sin(Δλ / 2) * Math.sin(Δλ / 2)
    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))

    val d = R * c // distance in meters
    d
  }

}
