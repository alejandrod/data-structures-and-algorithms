package sections.section9

//https://leetcode.com/problems/implement-queue-using-stacks/description/
class MyQueue {

  private val first = new scala.collection.mutable.Stack[Int]()
  private val last = new scala.collection.mutable.Stack[Int]()

  /** Initialize your data structure here. */


  /** Push element x to the back of queue. */
  def push(x: Int) {
    while (first.nonEmpty) {
      last.push(first.pop())
    }
    last.push(x)
  }

  /** Removes the element from in front of queue and returns that element. */
  def pop(): Int = {
    while (last.nonEmpty) {
      first.push(last.pop())
    }
    first.pop()
  }

  /** Get the front element. */
  def peek(): Int = {
    while (last.nonEmpty) {
      first.push(last.pop())
    }
    first.head
  }

  /** Returns whether the queue is empty. */
  def empty(): Boolean = {
    first.isEmpty && last.isEmpty
  }

}

object MyQueueApp extends App {
  var queue = new MyQueue()

  queue.push(1)
  queue.push(2)

  assert(queue.peek() == 1)
  assert(queue.pop() == 1)
  assert(!queue.empty())
}
