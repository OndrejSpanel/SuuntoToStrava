package net.suunto3rdparty

class MoveIndex {
  var index = Seq[Move]()

  def add(move: Move): Unit = index = move +: index

  /**
    * list all segments overlapping in time the given one
    */
  def listOverlapping(that: DataStream, streamType: StreamType): Seq[DataStream] = {
    val streamsWithMatchingType = index.flatMap(_.streams.get(streamType))
    streamsWithMatchingType.filter(_.isOverlapping(that))
  }
}