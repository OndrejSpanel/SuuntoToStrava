package net.suunto3rdparty

import java.text.SimpleDateFormat

import scala.collection.mutable

object SuuntoMove {
  val dateFormat: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  case class Header(startTime: String = "", duration: Int = 0, calories: Int = 0, distance: Int = 0)
  case class TrackPoint(latitude: Double, longitude: Double, elevation: Option[Int], time: String)
}

case class SuuntoMove(var startTime: String = "", var duration: Int = 0, var calories: Int = 0, var distance: Int = 0) {
  import SuuntoMove._

  private var distanceSamples = Seq[Int]()
  private var heartRateSamples = Seq[Int]()
  private var trackPoints = Seq[TrackPoint]()

  def this(distSamples: Seq[Int], hrSamples: Seq[Int]) = {
    this()
    distanceSamples ++= distSamples
    heartRateSamples ++= hrSamples
  }

  def this(header: SuuntoMove.Header, distSamples: Seq[Int], hrSamples: Seq[Int], trackPointSamples: Seq[SuuntoMove.TrackPoint]) = {
    this(header.startTime, header.duration, header.calories, header.distance)
    distanceSamples = distSamples
    heartRateSamples = hrSamples
    trackPoints = trackPointSamples
  }

  def this(header: SuuntoMove.Header, distSamples: Seq[Int], hrSamples: Seq[Int]) = {
    this(header, distSamples, hrSamples, Seq())
  }
}
