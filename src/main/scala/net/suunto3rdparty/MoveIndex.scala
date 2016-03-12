package net.suunto3rdparty

class MoveIndex {
  var index = Seq[SuuntoMove]()

  def add(move: SuuntoMove): Unit = index = move +: index

  /**
    * list all segments overlapping in time the given one
    */
  def listOverlapping(that: SuuntoMove): Seq[SuuntoMove] = index.filter(_.isOverlapping(that))
}