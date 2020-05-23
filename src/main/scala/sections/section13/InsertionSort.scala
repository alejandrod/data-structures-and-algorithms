package sections.section13

/**
  * Note, this is based on https://visualgo.net/en/sorting -> Insertion Sort
  * The one in the lecture is a bit inefficient because for cases where the array is mostly sorted, is better to start
  * from the elements closer to the current one.
  */
object InsertionSort {

  def sort(array: Array[Int]): Unit = {
    for (i <- array.indices) {
      val lastSorted = array(i)
      var j = i - 1
      var keepLooking = j >= 0

      while (keepLooking) {
        val current = array(j)
        if (current > lastSorted) {
          array(j + 1) = current
          keepLooking = j > 0
          if (keepLooking) {
            j -= 1
          }
        } else {
          j += 1 // We found something that is smaller.
          keepLooking = false
        }
      }

      if (j >= 0) {
        array(j) = lastSorted
      }
    }
  }

}

object InsertionSortApp extends App with SortTesting {

  testInplace(InsertionSort.sort)

}

