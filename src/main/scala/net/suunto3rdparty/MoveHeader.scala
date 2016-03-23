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

  def mergeDeviceNames(names: Set[String]): Option[String] = {
    if (names.size <= 1) names.headOption
    else {
      val combine = false
      object DeviceNameOrdering extends Ordering[String] {
        def score(name: String): Int = {
          if (name.contains("Quest")) 200
          else if (name.contains("GPS")) 100
          else if (name.contains("Foot")) 300
          else 400
        }
        override def compare(x: String, y: String): Int = {
          val d = score(x) compare score(y)
          if (d != 0) d else x compare y
        }
      }
      if (combine) {
        import collection.immutable.IndexedSeq
        val namesOrdered = names.toIndexedSeq.sorted(DeviceNameOrdering)
        val firstWords = names.map(_.takeWhile(!_.isSpaceChar))
        // try to use the first words as a brand
        def removeBrand(names: IndexedSeq[String], brand: String) = {
          names.head +: names.tail.map { name =>
            if (name.startsWith(brand)) name.drop(brand.length + 1)
            else name
          }
        }
        def totalChars(names: IndexedSeq[String]) = names.map(_.length).sum

        val removedBrand = firstWords.foldLeft(namesOrdered) { (best, word) =>
          val check = removeBrand(namesOrdered, word)
          if (totalChars(check) < totalChars(best)) check
          else best
        }
        Some(removedBrand.mkString(" + "))
      } else {
        // choose one only
        val best = names.min(DeviceNameOrdering)
        Some(best)
      }
    }
  }
}

case class MoveHeader(deviceNames: Set[String], moveType: MoveHeader.ActivityType = MoveHeader.ActivityType.RunningTrail) {
  def merge(header: MoveHeader): MoveHeader = {
    copy(deviceNames = deviceNames ++ header.deviceNames)
  }
}