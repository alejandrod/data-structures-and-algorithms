package sections.section9

//Note: we keep first == last if there is a single entry (vs None). Which makes more sense.
class Queue[T] {

  private case class Node(var value: T, var next: Option[Node] = None) {
    override def toString: String = value.toString
  }

  private var _first: Option[Node] = None
  private var _last: Option[Node] = _first
  private var _length: Int = 0

  def length: Int = _length

  def dequeue: Option[T] = {
    _first.map(first => {
      _first = first.next
      if(first.next == _last) {
        _last = _first
      }
      _length -= 1
      first.value
    })
  }

  def enqueue(e: T): Queue[T] = {
    val node = Node(e)

    if (_first.isEmpty) {
      _first = Some(node)
      _last = _first
    } else {
      _last.get.next = Some(node)
      _last = Some(node)
    }

    _length += 1
    this
  }

  def peek: Option[T] = {
    _first.map(_.value)
  }

}

object QueueApp extends App {
  val myQueue = new Queue[String]()

  assert(myQueue.dequeue.isEmpty)
  assert(myQueue.peek.isEmpty)
  assert(myQueue.length == 0)

  myQueue.enqueue("Joy")
  assert(myQueue.length == 1)
  assert(myQueue.peek.contains("Joy"))
  assert(myQueue.dequeue.contains("Joy"))
  assert(myQueue.length == 0)
  assert(myQueue.peek.isEmpty)

  myQueue.enqueue("Joy")
  myQueue.enqueue("Matt")
  myQueue.enqueue("Pavel")
  myQueue.enqueue("Samir")
  assert(myQueue.length == 4)
  assert(myQueue.peek.contains("Joy"))
  assert(myQueue.dequeue.contains("Joy"))
  assert(myQueue.length == 3)
  assert(myQueue.peek.contains("Matt"))

  assert(myQueue.dequeue.contains("Matt"))
  assert(myQueue.dequeue.contains("Pavel"))
  assert(myQueue.dequeue.contains("Samir"))
  assert(myQueue.dequeue.isEmpty)
  assert(myQueue.dequeue.isEmpty)
  assert(myQueue.length == 0)
}
