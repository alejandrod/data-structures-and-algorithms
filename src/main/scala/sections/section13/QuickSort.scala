package sections.section13

object QuickSort {

  def sort(array: Array[Int]): Unit = {
    quickSort(array, 0, array.length - 1)
  }

  private def quickSort(array: Array[Int], low: Int, high: Int): Unit = {
    if (low < high) {
      val m = partition(array, low, high) // O(N)
      // a[low..high] ~> a[low..m–1], pivot, a[m+1..high]
      quickSort(array, low, m - 1) // recursively sort left subarray
      // a[m] = pivot is already sorted after partition
      quickSort(array, m + 1, high) // then sort right subarray
    }
  }

  private def partition(array: Array[Int], i: Int, j: Int): Int = {
    val pivot = array(i) // This is the classic version. We pick the first one
    var partitionIndex = i

    for (k <- i + 1 to j) { // explore the unknown region
      if (array(k) < pivot) {
        partitionIndex += 1
        swap(array, k, partitionIndex)
      }
    }

    swap(array, i, partitionIndex)
    partitionIndex
  }

  private def swap[T](array: Array[T], i: Int, j: Int): Unit = {
    if (i != j) {
      val tmp = array(i)
      array(i) = array(j)
      array(j) = tmp
    }
  }
}

object QuickSortApp extends App with SortTesting {

  testInplace(QuickSort.sort)

}
