package sections.section13

object SelectionSort {

  def sort(array: Array[Int]): Unit = {
    val length = array.length

    for (i <- array.indices) {
      val current = array(i)
      var min = current
      var minIndex = i

      for (j <- LazyList.range(i + 1, length)) {
        val next = array(j)
        if (next < min) {
          min = next
          minIndex = j
        }
      }

      if (minIndex != i) {
        array(i) = min
        array(minIndex) = current
      }
    }
  }

}

object SelectionSortApp extends App with SortTesting {

  testInplace(SelectionSort.sort)

}
