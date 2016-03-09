package net.suunto3rdparty

import java.io.File

import org.w3c.dom.Element

object Util {
  def getChildElementValue(parent0: Element, elementNames: String*): String = {
    var parent = parent0
    for ((elementName, i) <- elementNames.zipWithIndex) {
      val nodeList = parent.getElementsByTagName(elementName)
      if (nodeList.getLength != 1) return null
      val child = nodeList.item(0).asInstanceOf[Element]
      if (i == elementNames.length - 1) {
        return child.getTextContent
      }
      parent = child
    }
    null
  }

  def isWindows: Boolean = {
    val OS = System.getProperty("os.name").toLowerCase
    OS.contains("win")
  }
  def isMac: Boolean = {
    val OS = System.getProperty("os.name").toLowerCase
    OS.contains("mac")
  }
  def isUnix: Boolean = {
    val OS = System.getProperty("os.name").toLowerCase
    OS.contains("nix") || OS.contains("nux") || OS.contains("aix")
  }
  def getSuuntoHome: File = {
    if (Util.isWindows) {
      val appData = System.getenv("APPDATA")
      return new File(new File(appData), "Suunto")
    }
    if (Util.isMac) {
      val userHome = System.getProperty("user.home")
      return new File(new File(userHome), "Library/Application Support/Suunto/")
    }
    if (Util.isUnix) {
      val userHome = System.getProperty("user.home")
      return new File(new File(userHome), "Suunto")
    }
    throw new UnsupportedOperationException("Unknown operating system")
  }
}
