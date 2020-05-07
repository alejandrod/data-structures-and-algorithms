package sections.section8

// Note: a list can be empty. For some reason the course says it doesn't make sense.
// But, on every language you can create empty collections.
class DoubleLinkedList[T] {

  private case class Node(var value: T, var previous: Option[Node] = None, var next: Option[Node] = None) {
    override def toString: String = value.toString
  }

  private var _head: Option[Node] = None
  private var _tail: Option[Node] = _head
  private var _length: Int = 0

  def head: Option[T] = _head.map(_.value)

  def tail: Option[T] = _tail.map(_.value)

  def length: Int = _length

  def append(e: T): DoubleLinkedList[T] = {
    if (_head.isEmpty) {
      _head = Some(Node(e))
      _tail = _head
    } else {
      val node = Node(e, previous = _tail)
      _tail.get.next = Some(node)
      _tail = Some(node)
    }

    _length += 1
    this
  }

  def prepend(e: T): DoubleLinkedList[T] = {
    if (_head.isEmpty) {
      _head = Some(Node(e))
      _tail = _head
    } else {
      val node = Node(e, next = _head)
      _head = Some(node)
    }

    _length += 1
    this
  }

  def insert(i: Int, e: T): DoubleLinkedList[T] = {
    if (i == 0) {
      prepend(e)
    } else if (i == _length - 1) {
      append(e)
    } else {
      traverse(i - 1).foreach(leader => {
        val node = Node(e, previous = Some(leader), next = leader.next)
        leader.next = Some(node)
        _length += 1
      })
    }

    this
  }

  def removeAt(i: Int): DoubleLinkedList[T] = {
    if (i == 0) {
      _head = _head.flatMap(_.next)
      _length -= 1
    } else {
      traverse(i - 1).foreach(leader => {
        leader.next = leader.next.flatMap(_.next)
        _length -= 1
      })
    }

    this
  }

  def apply(i: Int): Option[T] = traverse(i).map(_.value)

  def iterator: Iterator[T] = nodeIteratorHead.map(_.value)

  override def toString: String = nodeIteratorHead.map(_.value).toList.toString()

  // Iterators are lazy. So, we are good.
  // https://docs.scala-lang.org/overviews/collections/iterators.html
  private def nodeIteratorHead: Iterator[Node] = new Iterator[Node] {
    private var current = _head

    override def hasNext: Boolean = current.isDefined

    override def next(): Node = {
      val value = current.get
      current = current.get.next
      value
    }
  }

  private def nodeIteratorTail: Iterator[Node] = new Iterator[Node] {
    private var current = _tail

    override def hasNext: Boolean = current.isDefined

    override def next(): Node = {
      val value = current.get
      current = current.get.previous
      value
    }
  }

  private def traverse(i: Int): Option[Node] = {
    if (i < 0 && i >= length) {
      throw new IndexOutOfBoundsException("Index should be 0 <= i < length")
    }
    val middle = _length / 2
    if (i <= middle) {
      nodeIteratorHead.zipWithIndex.find(_._2 == i).map(_._1)
    } else {
      var idx = _length - 1
      nodeIteratorTail.find(_ => {
        if(idx == i) {
          true
        } else {
          idx -= 1
          false
        }
      })
    }
  }
}

object DoubleLinkedListApp extends App {

  val newDoubleLinkedList = new DoubleLinkedList[Int]()
  assert(newDoubleLinkedList.length == 0)
  newDoubleLinkedList.append(10)
  assert(newDoubleLinkedList.head.contains(10))
  assert(newDoubleLinkedList.tail.contains(10))
  assert(newDoubleLinkedList.length == 1)

  newDoubleLinkedList.append(20)
  newDoubleLinkedList.append(20)
  newDoubleLinkedList.append(30)

  assert(newDoubleLinkedList.head.contains(10))
  assert(newDoubleLinkedList.tail.contains(30))
  assert(newDoubleLinkedList.length == 4)

  newDoubleLinkedList.prepend(50)
  assert(newDoubleLinkedList.head.contains(50))
  assert(newDoubleLinkedList.tail.contains(30))
  assert(newDoubleLinkedList.length == 5)

  assert(newDoubleLinkedList.insert(0, 44).head.contains(44))
  assert(newDoubleLinkedList.length == 6)

  assert(newDoubleLinkedList.insert(newDoubleLinkedList.length - 1, 66).tail.contains(66))
  assert(newDoubleLinkedList.length == 7)

  assert(newDoubleLinkedList.insert(2, 77).iterator.toList(2) == 77)
  assert(newDoubleLinkedList.length == 8)

  assert(newDoubleLinkedList.removeAt(2).iterator.toList(2) == 10)
  assert(newDoubleLinkedList.length == 7)

  assert(newDoubleLinkedList.removeAt(0).head.contains(50))
  assert(newDoubleLinkedList.length == 6)


  assert(newDoubleLinkedList(4).contains(30))
  assert(newDoubleLinkedList(5).contains(66))
  assert(newDoubleLinkedList(1).contains(10))
  assert(newDoubleLinkedList(0).contains(50))

}
