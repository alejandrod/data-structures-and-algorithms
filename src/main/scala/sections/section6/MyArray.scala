package sections.section6

import scala.collection.mutable

/**
  * Dynamic array.
  * No exceptions are thrown if index is out of bounds
  *
  * Not thread safe!
  *
  * @tparam T anything you want
  */
class MyArray[T] {
  private val data = new mutable.TreeMap[Int, T]()
  private var _length = 0

  def length: Int = _length

  def get(index: Int): Option[T] = {
    if (index >= 0 && index < _length) {
      data.get(index)
    } else {
      None
    }
  }

  def push(item: T): Int = {
    data(_length) = item
    _length += 1
    _length
  }

  def pop: Option[T] = {
    if (_length > 0) {
      val last = data.get(_length - 1)
      last.foreach(_ => data.remove(_length - 1))
      _length -= 1
      last
    } else {
      None
    }
  }

  def deleteAtIndex(index: Int): Option[T] = {
    if (index >= 0 && index < _length) {
      val removed = data.remove(index)
      shiftItems(index)
      removed
    } else {
      None
    }
  }

  def shiftItems(index: Int): Int = {
    for {
      i <- Range(index, _length - 1)
    } yield {
      data(i) = data(i + 1)
    }
    data.remove(_length - 1)
    _length -= 1
    _length
  }

  override def toString: String = s"[${data.values.mkString(", ")}]"

  def mkString: String = data.values.mkString
}

object MyArrayApp extends App {

  val myArray = new MyArray[String]
  myArray.push("hi")
  myArray.push("you")
  myArray.push("!")
  myArray.pop
  myArray.deleteAtIndex(0)
  myArray.push("are")
  myArray.push("nice")

//  myArray.deleteAtIndex(1)
  myArray.shiftItems(0)

  println(myArray)
  println(myArray.length)

}
