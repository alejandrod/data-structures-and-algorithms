package sections.section13

/**
  * Different than the video. Refer to https://en.wikipedia.org/wiki/Bubble_sort
  * Using 2 nested for loops makes it lest performance
  * Think about the default case where there are sorted things. 2 loops will check them all O(n^2^)
  * Vs say O(n) if already sorted.
  */
object BubbleSort {

  def sort(array: Array[Int]): Unit = {
    val length = array.length
    var swapped = false

    do {
      swapped = false
      for {
        i <- array.indices if i + 1 < length
      } {
        val current = array(i)
        val next = array(i + 1)
        if (current > next) {
          array(i) = next
          array(i + 1) = current
          swapped = true
        }
      }
    } while (swapped)
  }

}

object BubbleSortApp extends App with SortTesting {

  testInplace(BubbleSort.sort)

}
