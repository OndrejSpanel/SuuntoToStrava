package net.suunto3rdparty

class MoveIndex {
  var index = Set[Move]()

  def add(move: Move): Unit = index = index + move

  /**
    * list all segments overlapping in time the given one
    */
  def listOverlapping(that: DataStream, streamType: StreamType): Set[(Move, DataStream)] = {
    val streamsWithMatchingType = index.flatMap(move => move.streams.get(streamType).map(move -> _))
    streamsWithMatchingType.filter(_._2.isOverlapping(that))
  }
}