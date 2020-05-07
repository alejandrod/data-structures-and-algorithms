package sections.section8

// Note: a list can be empty. For some reason the course says it doesn't make sense.
// But, on every language you can create empty collections.
class LinkedList[T] {

  private case class Node(var value: T, var next: Option[Node] = None) {
    override def toString: String = value.toString
  }

  private var _head: Option[Node] = None
  private var _tail: Option[Node] = _head
  private var _length: Int = 0

  def head: Option[T] = _head.map(_.value)

  def tail: Option[T] = _tail.map(_.value)

  def length: Int = _length

  def append(e: T): LinkedList[T] = {
    val node = Node(e)

    if (_head.isEmpty) {
      _head = Some(node)
      _tail = _head
    } else {
      _tail.get.next = Some(node)
      _tail = Some(node)
    }

    _length += 1
    this
  }

  def prepend(e: T): LinkedList[T] = {
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

  def insert(i: Int, e: T): LinkedList[T] = {
    if (i == 0) {
      prepend(e)
    } else if (i == _length - 1) {
      append(e)
    } else {
      traverse(i - 1).foreach(leader => {
        val node = Node(e, next = leader.next)
        leader.next = Some(node)
        _length += 1
      })
    }

    this
  }

  def removeAt(i: Int): LinkedList[T] = {
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

  def iterator: Iterator[T] = nodeIterator.map(_.value)

  def reverse: LinkedList[T] = {
    var previous: Option[Node] = None
    nodeIterator.foreach(currentNode => {
      currentNode.next = previous
      previous = Some(currentNode)
    })
    _tail = _head
    _head = previous
    this
  }

  override def toString: String = nodeIterator.map(_.value).toList.toString()

  // Iterators are lazy. So, we are good.
  // https://docs.scala-lang.org/overviews/collections/iterators.html
  private def nodeIterator: Iterator[Node] = new Iterator[Node] {
    private var current = _head

    override def hasNext: Boolean = current.isDefined

    override def next(): Node = {
      val value = current.get
      current = current.get.next
      value
    }
  }

  private def traverse(i: Int): Option[Node] = {
    if (i < 0 && i >= length) {
      throw new IndexOutOfBoundsException("Index should be 0 <= i < length")
    }
    nodeIterator.zipWithIndex.find(_._2 == i).map(_._1)
  }
}

object LinkedListApp extends App {

  val newLinkedList = new LinkedList[Int]()
  assert(newLinkedList.length == 0)
  newLinkedList.append(10)
  assert(newLinkedList.head.contains(10))
  assert(newLinkedList.tail.contains(10))
  assert(newLinkedList.length == 1)

  newLinkedList.append(20)
  newLinkedList.append(20)
  newLinkedList.append(30)

  assert(newLinkedList.head.contains(10))
  assert(newLinkedList.tail.contains(30))
  assert(newLinkedList.iterator.toList == List(10, 20, 20, 30))
  assert(newLinkedList.length == 4)

  newLinkedList.prepend(50)
  assert(newLinkedList.head.contains(50))
  assert(newLinkedList.tail.contains(30))
  assert(newLinkedList.length == 5)

  assert(newLinkedList.insert(0, 44).head.contains(44))
  assert(newLinkedList.length == 6)

  assert(newLinkedList.insert(newLinkedList.length - 1, 66).tail.contains(66))
  assert(newLinkedList.length == 7)

  assert(newLinkedList.insert(2, 77).iterator.toList(2) == 77)
  assert(newLinkedList.length == 8)

  assert(newLinkedList.removeAt(2).iterator.toList(2) == 10)
  assert(newLinkedList.length == 7)

  assert(newLinkedList.removeAt(0).head.contains(50))
  assert(newLinkedList.length == 6)

  //List(50, 10, 20, 20, 30, 66)
  assert(newLinkedList.reverse.iterator.toList == List(66, 30, 20, 20, 10, 50))

  val newLinkedList1 = new LinkedList[Int]()
  newLinkedList1.append(101)
  assert(newLinkedList1.reverse.iterator.toList == List(101))

  val newLinkedListEmpty = new LinkedList[Int]()
  assert(newLinkedListEmpty.reverse.iterator.toList == Nil)
  assert(newLinkedListEmpty.head.isEmpty)
  assert(newLinkedListEmpty.tail.isEmpty)

}
