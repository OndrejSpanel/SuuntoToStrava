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

  def mergeDeviceNames(dn1: String, dn2: String): String = {
    if (dn1 == dn2) dn1
    else {
      // prefer GPS information
      if (dn1.contains("GPS")) dn1
      else dn2
    }
  }
}

case class MoveHeader(deviceName: Option[String], moveType: MoveHeader.ActivityType = MoveHeader.ActivityType.RunningTrail) {
  import MoveHeader._

  def merge(header: MoveHeader): MoveHeader = {
    val devName = (deviceName, header.deviceName) match {
      case (Some(dn1), Some(dn2)) =>
        Some(mergeDeviceNames(dn1, dn2))
      case (Some(dn), None) => Some(dn)
      case (None, Some(dn)) => Some(dn)
      case _ => None
    }
    copy(deviceName = devName)
  }
}