package sections.section9

class StackArray[T](bufferIncreaseSize: Int = 32) {

  private var data: Array[Option[T]] = Array.fill(bufferIncreaseSize)(None)
  private var position = 0

  def length: Int = position + 1

  def pop: Option[T] = {
    if (position >= 0) {
      val v = data(position)
      data(position) = None
      position -= 1
      v
    } else {
      None
    }
  }

  def push(e: T): StackArray[T] = {
    position += 1
    if (position >= data.length) {
      val newData: Array[Option[T]] = Array.fill(data.length + bufferIncreaseSize)(None)
      Array.copy(data, 0, newData, 0, data.length)
      data = newData
    }
    data(position) = Some(e)

    this
  }

  def peek: Option[T] = {
    if (position >= 0) {
      data(position)
    } else {
      None
    }
  }

}

object StackArrayApp extends App {
  val myStack = new StackArray[String](4)

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

  // check array increase
  myStack.push("Google1")
  myStack.push("Google2")
  myStack.push("Google3")
  myStack.push("Google4")
  myStack.push("Google5")
  myStack.push("Google6")
  myStack.push("Google7")
  assert(myStack.length == 7)
  assert(myStack.peek.contains("Google7"))
  assert(myStack.pop.contains("Google7"))
  assert(myStack.length == 6)
}
