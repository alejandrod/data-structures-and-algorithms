package sections.section15

object DynamicProgramming {
  var calculations = 0

  def fibonacci(n: Long): Long = {
    calculations += 1

    if (n < 2) {
      n
    } else {
      fibonacci(n - 1) + fibonacci(n - 2)
    }
  }

  def fibonacciMaster: Long => Long = {
    val cache = new scala.collection.mutable.HashMap[Long, Long]()

    def fibonacci(n: Long): Long = {
      calculations += 1

      if (n < 2) {
        n
      } else {
        cache.getOrElseUpdate(n - 1, fibonacci(n - 1)) + cache.getOrElseUpdate(n - 2, fibonacci(n - 2))
      }
    }

    fibonacci
  }

  def fibonacciMasterBottomUp(n: Long): Long = {
    calculations += 1
    val stack = new scala.collection.mutable.Stack[Long]()
    stack.push(0L, 1L)

    (2L to n).foreach(_ => stack.push(stack.head + stack.tail.head))
    stack.pop()
  }

}

object DynamicProgrammingApp extends App {

  import DynamicProgramming._

  val fibonacciMemoized = fibonacciMaster

  calculations = 0
  assert(fibonacciMemoized(35) == 9227465L)
  println(s"fibonacciMemoized 35: - $calculations")
  println(s"fibonacciMemoized 35: - $calculations")

  calculations = 0
  assert(fibonacciMemoized(70) == 190392490709135L)
  println(s"fibonacciMemoized 70: - $calculations")

  calculations = 0
  assert(fibonacciMasterBottomUp(70) == 190392490709135L)
  println(s"fibonacciMasterBottomUp 70: - $calculations")
}
