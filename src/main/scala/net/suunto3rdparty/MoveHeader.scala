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
}
case class MoveHeader(moveType: String = "Run")