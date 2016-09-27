package net.suunto3rdparty

import java.io.{File, FileInputStream}
import java.util.Properties

import moveslink.MovesLinkUploader
import resource._


object Settings {
  private val settingsFile = "suuntoToStrava.cfg"

  private val file = new File(MovesLinkUploader.getDataFolder, settingsFile)
  private var props: Properties = new Properties()
  for (f <- managed(new FileInputStream(file))) {
    props = new Properties()
    props.load(f)
  }

  val questTimeOffset: Int = props.getProperty("questTimeOffset", "0").toInt
  val maxHR: Int = props.getProperty("maxHR", "240").toInt

}


