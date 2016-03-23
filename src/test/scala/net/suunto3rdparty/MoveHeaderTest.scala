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
  it should "merge identical names" in {
    mergeDeviceNames(gps, gps) shouldBe gps
    mergeDeviceNames(quest, quest) shouldBe quest
  }

  it should "merge different names" in {
    mergeDeviceNames(gps, quest) shouldBe gps
    mergeDeviceNames(quest, gps) shouldBe gps
  }
}
