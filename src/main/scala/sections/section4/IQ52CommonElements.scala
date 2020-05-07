package sections.section4

object IQ52CommonElements {
  // Given 2 arrays find if there are common items
  // Input are characters or any comparable object
  // output true or false

  def hasCommonElementsQuadratic[T](arrayA: Array[T], arrayB: Array[T]): Boolean = {
    val r = for {
      a <- arrayA.to(LazyList)
      b <- arrayB.to(LazyList)
    } yield {
      a == b
    }

    r.contains(true)
  }

  def hasCommonElementsLinear[T](arrayA: Array[T], arrayB: Array[T]): Boolean = {
    val setA = arrayA.toSet //O(n)
    arrayB.exists(setA.contains)
  }

}

object IQ52CommonElementsApp extends App {

  import IQ52CommonElements._

  val array1 = Array('a', 'b', 'c', 'x')

  val array2 = Array('z', 'y', 'i')

  val array3 = Array('z', 'y', 'x')

  println("N2")
  println(hasCommonElementsQuadratic(array1, array2))
  println(hasCommonElementsQuadratic(array1, array3))

  println("N")
  println(hasCommonElementsLinear(array1, array2))
  println(hasCommonElementsLinear(array1, array3))
}
