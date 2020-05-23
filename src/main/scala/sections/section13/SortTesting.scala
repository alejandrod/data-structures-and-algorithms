package sections.section13

trait SortTesting {

  def testInplace(inPlaceSort: Array[Int] => Unit): Unit = {
    inPlaceSort(Array[Int]())

    val single = Array[Int](5)
    inPlaceSort(single)
    assert(single sameElements Array(5))

    val two = Array[Int](8, 4)
    inPlaceSort(two)
    assert(two sameElements Array(4, 8))

    val three = Array[Int](8, 4, 0)
    inPlaceSort(three)
    assert(three sameElements Array(0, 4, 8))

    val multiple = Array[Int](8, 4, 0, 1, 2, 10, 7, 7)
    inPlaceSort(multiple)
    assert(multiple sameElements Array(0, 1, 2, 4, 7, 7, 8, 10))

    val multiple2 = Array[Int](3, 4, 0, 1, 8, 10, 1, 7)
    inPlaceSort(multiple2)
    assert(multiple2 sameElements Array(0, 1, 1, 3, 4, 7, 8, 10))

    val multiple3 = Array[Int](2, 2, 2, 2, 2, 2, 2, 1)
    inPlaceSort(multiple3)
    assert(multiple3 sameElements Array(1, 2, 2, 2, 2, 2, 2, 2))
  }

}
