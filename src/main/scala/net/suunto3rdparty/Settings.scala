package net.suunto3rdparty

import java.util.Properties

class Settings(props: Properties) {
  val questTimeOffset: Int = props.getProperty("questTimeOffset", "0").toInt
  val maxHR = props.getProperty("maxHR", "240").toInt
}

