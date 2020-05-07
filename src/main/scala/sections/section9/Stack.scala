package sections.section9

//Note: bottom, as is in the course, is a bit useless unless iterators or something else is added
class Stack[T] {

  private case class Node(var value: T, var next: Option[Node] = None) {
    override def toString: String = value.toString
  }

  private var _top: Option[Node] = None
  private var _length: Int = 0

  def length: Int = _length

  def pop: Option[T] = {
    _top.map(top => {
      _top = top.next
      _length -= 1
      top.value
    })
  }

  def push(e: T): Stack[T] = {
    val node = Node(e, _top)
    _top = Some(node)
    _length += 1
    this
  }

  def peek: Option[T] = {
    _top.map(_.value)
  }

}

object StackApp extends App {
  val myStack = new Stack[String]()

  assert(myStack.pop.isEmpty)
  assert(myStack.peek.isEmpty)
  assert(myStack.length == 0)

  myStack.push("Google")
  assert(myStack.length == 1)
  assert(myStack.peek.contains("Google"))
  assert(myStack.pop.contains("Google"))
  assert(myStack.length == 0)
  assert(myStack.peek.isEmpty)

  myStack.push("Google")
  myStack.push("Udemy")
  myStack.push("Discord")
  assert(myStack.length == 3)
  assert(myStack.peek.contains("Discord"))
  assert(myStack.pop.contains("Discord"))
  assert(myStack.length == 2)
  assert(myStack.peek.contains("Udemy"))

  assert(myStack.pop.contains("Udemy"))
  assert(myStack.pop.contains("Google"))
  assert(myStack.pop.isEmpty)
  assert(myStack.pop.isEmpty)
  assert(myStack.length == 0)
}
