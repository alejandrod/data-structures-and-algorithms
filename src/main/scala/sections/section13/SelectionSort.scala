package sections.section13

object SelectionSort {

  def sort[T <: Comparable[T]](array: Array[T]): Unit = {
    val length = array.length

    for (i <- array.indices) {
      val current = array(i)
      var min = current
      var minIndex = i

      for (j <- LazyList.range(i + 1, length)) {
        val next = array(j)
        if (next.compareTo(min) < 0) {
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

object SelectionSortApp extends App {

  import SelectionSort._

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
