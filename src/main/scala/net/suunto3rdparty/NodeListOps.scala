package net.suunto3rdparty

import org.w3c.dom.{Node, NodeList}

object NodeListOps {
  // TODO: replace with scala.xml.Node
  implicit class NodeListToSeq(val nodeList: NodeList) {
    def toSeq: Seq[Node] = {
      val count = nodeList.getLength
      for (i <- 0 until count) yield nodeList.item(i)
    }
  }
}
