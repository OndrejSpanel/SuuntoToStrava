package net.suunto3rdparty

import java.io.{File, FileInputStream, FileOutputStream}
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

  /**
    * how much are Quest watches are ahead of real time
    * real time = quest time - questTimeOffset
  Example:
    Current time	14:51:49
    Quest time	14:52:18
    Quest time offset 29
  */

  var questTimeOffset: Int = props.getProperty("questTimeOffset", "0").toInt
  var maxHR: Int = props.getProperty("maxHR", "240").toInt

  def save(newMaxHR: Option[Int], newQuestTimeOffset: Option[Int]): Unit = {
    newMaxHR.foreach { v =>
      props.setProperty("maxHR", v.toString)
      questTimeOffset = v;
    }
    newQuestTimeOffset.foreach { v =>
      props.setProperty("questTimeOffset", v.toString)
      maxHR = v;
    }
    for (f <- managed(new FileOutputStream(file))) {
      props.store(f, "SuuntoToStrava configuration")
      Console.println("Settings saved")
    }
  }
}


