package net.suunto3rdparty

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Ondra on 23.3.2016.
  */
class MoveHeaderTest extends FlatSpec with Matchers {
  import MoveHeader._

  behavior of "MoveHeaderTest"

  val quest = "Suunto Quest"
  val gps = "Suunto GPS Track Pod"
  val foot = "Suunto Foot Pod Mini"

  it should "merge identical names" in {
    mergeDeviceNames(Set(gps, gps)) shouldBe Some(gps)
    mergeDeviceNames(Set(quest, quest)) shouldBe Some(quest)
  }

  it should "merge different names" in {
    val questAndGPS = "Suunto Quest + GPS Track Pod"
    val questAndFootAndGPS = "Suunto Quest + GPS Track Pod + Foot Pod Mini"
    mergeDeviceNames(Set(gps, quest)) shouldBe Some(gps)
    mergeDeviceNames(Set(quest, gps)) shouldBe Some(gps)
    mergeDeviceNames(Set(quest, foot, gps)) shouldBe Some(gps)
    mergeDeviceNames(Set(quest, gps, foot)) shouldBe Some(gps)
  }
}
