package sections.section13

object InsertionSort {

  def sort[T <: Comparable[T]](array: Array[T]): Unit = {
    for (i <- array.indices) {
      val current = array(i)

      var j = i - 1
      var found = false
      while (j >= 0 && !found) {
        val next = array(j)
        if (current.compareTo(next) < 0) {
          j -= 1
        } else {
          found = true
        }
      }

      if (j >= 0 && j != i) {
        array(i) = array(j)
        array(j) = current
      }
    }
  }

}

object InsertionSortApp extends App {

  import InsertionSort._

  sort(Array())
//
//  val single = Array[Integer](5)
//  sort(single)
//  assert(single sameElements Array(5))
//
//  val two = Array[Integer](8, 4)
//  sort(two)
//  assert(two sameElements Array(4, 8))

  val three = Array[Integer](8, 4, 0)
  sort(three)
  assert(three sameElements Array(0, 4, 8))

  val multiple = Array[Integer](8, 4, 0, 1, 2, 10, 7)
  sort(multiple)
  assert(multiple sameElements Array(0, 1, 2, 4, 7, 8, 10))

}
