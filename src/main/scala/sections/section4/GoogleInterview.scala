package sections.section4

import scala.annotation.tailrec
import scala.collection.mutable

object GoogleInterview {

  // PROBLEM
  // 52: Google interview: https://www.youtube.com/watch?v=XKu_SEDAykw
  // find matching pair == some sum

  val example1 = Array(1, 2, 3, 9) // sum 8

  val example2 = Array(1, 2, 4, 4) // sum 8

  val example3 = Array(4, 1, 4, 2) // sum 8


  // SOLUTION
  // Input: sorted array of ints
  // Output: all pairs

  def findMatchingPairN2(array: Array[Int], sum: Int): Option[(Int, Int)] = {
    val r = for {
      i <- LazyList.range(0, array.length)
      j <- LazyList.range(i + 1, array.length) if array(i) + array(j) == sum
    } yield {
      (i, j)
    }

    r.headOption
  }

  def findMatchingPairInSortedN(array: Array[Int], sum: Int): Option[(Int, Int)] = {
    var i = 0
    var j = array.length - 1

    while (i < j) {
      val s = array(i) + array(j)
      if (s == sum) {
        return Some((i, j))
      } else if (s < sum) {
        i += 1
      } else {
        j -= 1
      }
    }

    None
  }

  def findMatchingPairInUnsortedN(array: Array[Int], sum: Int): Boolean = {
    val complements = mutable.HashSet.empty[Int]
    array.exists(e => {
      val c = sum - e
      if (complements.contains(c)) {
        true
      } else {
        complements.add(c)
        false
      }
    })
  }

  def findMatchingPairInUnsortedFunctionalN(array: Array[Int], sum: Int): Boolean = {
    array.to(LazyList).scanLeft(Tuple2(false, Set.empty[Int]))((acc, e) => {
      val c = sum - e
      if (acc._2.contains(c)) {
        (true, acc._2)
      } else {
        (false, acc._2 + c)
      }
    }).exists(_._1)
  }

  def findMatchingPairInUnsortedRecursiveN(array: Array[Int], sum: Int): Boolean = {
    @tailrec
    def loop(i: Int, complements: Set[Int]): Boolean = {
      if (i < array.length) {
        val e = array(i)
        val c = sum - e
        if (complements.contains(c)) {
          true
        } else {
          loop(i + 1, complements + c)
        }
      } else {
        false
      }
    }

    loop(0, Set.empty)
  }

}

object GoogleInterviewApp extends App {

  import sections.section4.GoogleInterview._

  println("N2")
  println(findMatchingPairN2(example1, 8))
  println(findMatchingPairN2(example2, 8))

  println("N")
  println(findMatchingPairInSortedN(example1, 8))
  println(findMatchingPairInSortedN(example2, 8))

  println("Unsorted N")
  println(findMatchingPairInUnsortedN(example1, 8))
  println(findMatchingPairInUnsortedN(example2, 8))
  println(findMatchingPairInUnsortedN(example3, 8))

  println("Unsorted N FP")
  println(findMatchingPairInUnsortedFunctionalN(example1, 8))
  println(findMatchingPairInUnsortedFunctionalN(example2, 8))
  println(findMatchingPairInUnsortedFunctionalN(example3, 8))

  println("Unsorted N FP recursive")
  println(findMatchingPairInUnsortedRecursiveN(example1, 8))
  println(findMatchingPairInUnsortedRecursiveN(example2, 8))
  println(findMatchingPairInUnsortedRecursiveN(example3, 8))
}
