package sections.section12

import scala.annotation.tailrec

object Recursion {

  def factorial(n: Int): Int = {
    @tailrec
    def loop(v: Int, acc: Int): Int = {
      if (v <= 1) {
        acc
      } else {
        loop(v - 1, v * acc)
      }
    }

    loop(n, 1)
  }

  def fibonacci(n: Int): Int = {
    @tailrec
    def loop(current: Int = 0, previous: Int = 1, i: Int = 0): Int = {
      if (current == n) {
        i
      } else if (current > n) {
        -1
      } else {
        val next = current + previous
        loop(next, current, i + 1)
      }
    }

    loop()
  }

  // In scala is better to use @tailrec
  // The reason is that it more performance. The Naive version is O(2^n) !!!!
  def fibonacciNaive(n: Int): Int = {
    if (n < 2) {
      n
    } else {
      fibonacciNaive(n - 1) + fibonacciNaive(n - 2)
    }
  }

  def reverse(n: String): String = {
    @tailrec
    def loop(acc: String, i: Int): String = {
      if(i < 0) {
        acc
      } else {
        loop(acc + n(i), i - 1)
      }
    }

    loop("", n.length - 1)
  }
}

object RecursionApp extends App {

  import Recursion._

  assert(factorial(0) == 1)
  assert(factorial(1) == 1)
  assert(factorial(2) == 2)
  assert(factorial(5) == 120)


  assert(fibonacci(0) == 0)
  assert(fibonacci(1) == 1)
  assert(fibonacci(3) == 4)
  assert(fibonacci(8) == 6)
  assert(fibonacci(4) == -1)

  val startTailrec = System.currentTimeMillis()
  fibonacci(43)
  val endTailrec = System.currentTimeMillis()
  println(s"Tailrec took ${endTailrec - startTailrec} millis")

  val startNaive = System.currentTimeMillis()
  fibonacciNaive(43)
  val endNaive = System.currentTimeMillis()
  println(s"Naive took ${endNaive - startNaive} millis")


  assert(reverse("alejandro") == "ordnajela")
  assert(reverse("yoyo master") == "retsam oyoy")
}
