package sections.section13

/**
  * Different than the video. Refer to https://en.wikipedia.org/wiki/Bubble_sort
  * Using 2 nested for loops makes it lest performance
  * Think about the default case where there are sorted things. 2 loops will check them all O(n^2^)
  * Vs say O(n) if already sorted.
  */
object BubbleSort {

  def sort[T <: Comparable[T]](array: Array[T]): Unit = {
    val length = array.length
    var swapped = false

    do {
      swapped = false
      for {
        i <- array.indices if i + 1 < length
      } {
        val current = array(i)
        val next = array(i + 1)
        if (current.compareTo(next) > 0) {
          array(i) = next
          array(i + 1) = current
          swapped = true
        }
      }
    } while (swapped)
  }

}

object BubbleSortApp extends App {

  import BubbleSort._

  sort(Array())

  val single = Array[Integer](5)
  sort(single)
  assert(single sameElements Array(5))

  val two = Array[Integer](8, 4)
  sort(two)
  assert(two sameElements Array(4, 8))

  val three = Array[Integer](8, 4, 0)
  sort(three)
  assert(three sameElements Array(0, 4, 8))

  val multiple = Array[Integer](8, 4, 0, 1, 2, 10, 7, 7)
  sort(multiple)
  assert(multiple sameElements Array(0, 1, 2, 4, 7, 7, 8, 10))

}
