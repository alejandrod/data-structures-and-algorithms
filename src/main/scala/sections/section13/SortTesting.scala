package sections.section13

trait SortTesting {

  def testInplace(inPlaceSort: Array[Integer] => Unit): Unit = {
//    inPlaceSort(Array[Integer]())
//
//    val single = Array[Integer](5)
//    inPlaceSort(single)
//    assert(single sameElements Array(5))
//
//    val two = Array[Integer](8, 4)
//    inPlaceSort(two)
//    assert(two sameElements Array(4, 8))
//
//    val three = Array[Integer](8, 4, 0)
//    inPlaceSort(three)
//    assert(three sameElements Array(0, 4, 8))

    val multiple = Array[Integer](8, 4, 0, 1, 2, 10, 7, 7)
    inPlaceSort(multiple)
    assert(multiple sameElements Array(0, 1, 2, 4, 7, 7, 8, 10))
  }

}
