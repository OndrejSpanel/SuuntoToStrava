package net.suunto3rdparty

object MoveHeader {
  sealed trait ActivityType
  object ActivityType {
    object RunningTrail extends ActivityType
    object RunningRoad extends ActivityType

    object Orienteering extends ActivityType

    object MountainBike extends ActivityType
    object Cycling extends ActivityType

    object Unknown extends ActivityType

  }

  def mergeDeviceNames(dns: Set[String]): Option[String] = {
    if (dns.isEmpty) None
    else if (dns.tail.isEmpty) dns.headOption
    else {
      // prefer GPS information
      val gps = dns.find(_.contains("GPS"))
      Some(gps.getOrElse(gps.head))
    }
  }
}

case class MoveHeader(deviceNames: Set[String], moveType: MoveHeader.ActivityType = MoveHeader.ActivityType.RunningTrail) {
  import MoveHeader._

  def merge(header: MoveHeader): MoveHeader = {
    copy(deviceNames = deviceNames ++ header.deviceNames)
  }
}