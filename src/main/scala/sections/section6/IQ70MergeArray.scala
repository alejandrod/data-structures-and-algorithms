package sections.section6

import java.util

object IQ70MergeArray {

  /**
    *  Best: O(1)
    *  Worst: O(n)
    */
  def mergeSortedArrays(arrayA: Array[Int], arrayB: Array[Int]): Array[Int] = {
    if (arrayA.isEmpty) {
      arrayB
    } else if (arrayB.isEmpty) {
      arrayA
    } else {
      val result = new Array[Int](arrayA.length + arrayB.length)
      var a, b = 0
      for {
        i <- result.indices
      } {
        if (a < arrayA.length &&
          (b == arrayB.length || (b < arrayB.length && arrayA(a) <= arrayB(b)))) {
          result(i) = arrayA(a)
          a += 1
        } else if (b < arrayB.length) {
          result(i) = arrayB(b)
          b += 1
        }
      }
      result
    }
  }

}

object IQ70MergeArrayApp extends App {

  import IQ70MergeArray._

  println(util.Arrays.toString(mergeSortedArrays(Array(0, 3, 4, 31), Array(3, 4, 6, 30))))
}
