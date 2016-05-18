package net.suunto3rdparty

import java.io.{File, FileInputStream}
import java.util.Properties

import resource._

object Settings {
  def load(props: Properties): Settings = {
    val offset = props.getProperty("questTimeOffset")
    val maxHR = props.getProperty("maxHR", "240")
    Settings(offset.toInt, maxHR.toInt)
  }
}
case class Settings(questTimeOffset: Int, maxHR: Int)
