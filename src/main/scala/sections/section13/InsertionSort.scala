package sections.section13

/**
  * Note, this is based on https://visualgo.net/en/sorting -> Insertion Sort
  * The one in the lecture is a bit inefficient because for cases where the array is mostly sorted, is better to start
  * from the elements closer to the current one.
  */
object InsertionSort {

  def sort[T <: Comparable[T]](array: Array[T]): Unit = {
    for (i <- array.indices) {
      val lastSorted = array(i)
      var j = i - 1
      var keepLooking = j >= 0

      while (keepLooking) {
        val current = array(j)
        if (current.compareTo(lastSorted) > 0) {
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

object InsertionSortApp extends App {

  import InsertionSort._

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
